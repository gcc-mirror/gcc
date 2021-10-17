#include "rust-mangle.h"
#include "fnv-hash.h"
#include <algorithm>

// FIXME: Rename those to legacy_*
static const std::string kMangledSymbolPrefix = "_ZN";
static const std::string kMangledSymbolDelim = "E";
static const std::string kMangledGenericDelim = "$C$";
static const std::string kMangledSubstBegin = "$LT$";
static const std::string kMangledSubstEnd = "$GT$";

namespace Rust {
namespace Compile {

Mangler::MangleVersion Mangler::version = MangleVersion::LEGACY;

static std::string
legacy_mangle_name (const std::string &name)
{
  return std::to_string (name.size ()) + name;
}

static std::string
legacy_mangle_canonical_path (const Resolver::CanonicalPath &path)
{
  std::string buffer;
  path.iterate_segs ([&] (const Resolver::CanonicalPath &p) -> bool {
    buffer += legacy_mangle_name (p.get ());
    return true;
  });
  return buffer;
}

// rustc uses a sip128 hash for legacy mangling, but an fnv 128 was quicker to
// implement for now
static std::string
legacy_hash (const std::string &fingerprint)
{
  Hash::FNV128 hasher;
  hasher.write ((const unsigned char *) fingerprint.c_str (),
		fingerprint.size ());

  uint64_t hi, lo;
  hasher.sum (&hi, &lo);

  char hex[16 + 1];
  memset (hex, 0, sizeof hex);
  snprintf (hex, sizeof hex, "%08" PRIx64 "%08" PRIx64, lo, hi);

  return "h" + std::string (hex, sizeof (hex) - 1);
}

static std::string
legacy_mangle_self (const TyTy::BaseType *self)
{
  if (self->get_kind () != TyTy::TypeKind::ADT)
    return legacy_mangle_name (self->get_name ());

  const TyTy::ADTType *s = static_cast<const TyTy::ADTType *> (self);
  std::string buf = s->get_identifier ();

  if (s->has_subsititions_defined ())
    {
      buf += kMangledSubstBegin;

      const std::vector<TyTy::SubstitutionParamMapping> &params
	= s->get_substs ();
      for (size_t i = 0; i < params.size (); i++)
	{
	  const TyTy::SubstitutionParamMapping &sub = params.at (i);
	  buf += sub.as_string ();

	  if ((i + 1) < params.size ())
	    buf += kMangledGenericDelim;
	}

      buf += kMangledSubstEnd;
    }

  return legacy_mangle_name (buf);
}

static std::string
v0_tuple_prefix (const TyTy::BaseType *ty)
{
  if (ty->is_unit ())
    return "u";

  // FIXME: ARTHUR: Add rest of algorithm
  return "";
}

static std::string
v0_numeric_prefix (const TyTy::BaseType *ty)
{
  static const std::map<std::string, std::string> num_prefixes = {
    {"[i8]", "a"},    {"[u8]", "h"},	{"[i16]", "s"}, {"[u16]", "t"},
    {"[i32]", "l"},   {"[u32]", "m"},	{"[i64]", "x"}, {"[u64]", "y"},
    {"[isize]", "i"}, {"[usize]", "j"}, {"[f32]", "f"}, {"[f64]", "d"},
  };

  auto ty_kind = ty->get_kind ();
  auto ty_str = ty->as_string ();
  auto numeric_iter = num_prefixes.end ();

  // Special numeric types
  if (ty_kind == TyTy::TypeKind::ISIZE)
    return "i";
  else if (ty_kind == TyTy::TypeKind::USIZE)
    return "j";

  numeric_iter = num_prefixes.find (ty_str);
  if (numeric_iter != num_prefixes.end ())
    return numeric_iter->second;

  return "";
}

static std::string
v0_simple_type_prefix (const TyTy::BaseType *ty)
{
  switch (ty->get_kind ())
    {
    case TyTy::TypeKind::BOOL:
      return "b";
    case TyTy::TypeKind::CHAR:
      return "c";
    case TyTy::TypeKind::STR:
      return "e";
    case TyTy::TypeKind::NEVER:
      return "z";

      // Placeholder types
    case TyTy::TypeKind::ERROR:	      // Fallthrough
    case TyTy::TypeKind::INFER:	      // Fallthrough
    case TyTy::TypeKind::PLACEHOLDER: // Fallthrough
    case TyTy::TypeKind::PARAM:
      // FIXME: TyTy::TypeKind::BOUND is also a valid variant in rustc
      return "p";

    case TyTy::TypeKind::TUPLE:
      return v0_tuple_prefix (ty);

    case TyTy::TypeKind::UINT:	// Fallthrough
    case TyTy::TypeKind::INT:	// Fallthrough
    case TyTy::TypeKind::FLOAT: // Fallthrough
    case TyTy::TypeKind::ISIZE: // Fallthrough
    case TyTy::TypeKind::USIZE: // Fallthrough
      return v0_numeric_prefix (ty);

    default:
      return "";
    }

  gcc_unreachable ();
}

// FIXME: Is this present somewhere in libbiberty already?
static std::string
v0_base62_integer (uint64_t x)
{
  const static std::string base_64
    = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@$";
  std::string buffer (128, '\0');
  size_t idx = 0;
  size_t base = 62;

  do
    {
      buffer[idx] = base_64[(x % base)];
      idx++;
      x = x / base;
    }
  while (x != 0);

  std::reverse (buffer.begin (), buffer.begin () + idx);
  return buffer.substr (0, idx);
}

// Add an underscore-terminated base62 integer to the mangling string.
// This corresponds to the `<base-62-number>` grammar in the v0 mangling RFC:
//  - 0 is encoded as "_"
//  - any other value is encoded as itself minus one in base 62, followed by "_"
static void
v0_add_integer_62 (std::string &mangled, uint64_t x)
{
  if (x > 0)
    mangled.append (v0_base62_integer (x - 1));

  mangled.append ("_");
}

// Add a tag-prefixed base62 integer to the mangling string when the
// integer is greater than 0:
//  - 0 is encoded as "" (nothing)
//  - any other value is encoded as <tag> + v0_add_integer_62(itself), that is
//  <tag> + base62(itself - 1) + '_'
static void
v0_add_opt_integer_62 (std::string &mangled, std::string tag, uint64_t x)
{
  if (x > 0)
    {
      mangled.append (tag);
      v0_add_integer_62 (mangled, x);
    }
}

static void
v0_add_disambiguator (std::string &mangled, uint64_t dis)
{
  v0_add_opt_integer_62 (mangled, "s", dis);
}

// Add an identifier to the mangled string. This corresponds to the
// `<identifier>` grammar in the v0 mangling RFC.
static void
v0_add_identifier (std::string &mangled, const std::string &identifier)
{
  // FIXME: gccrs cannot handle unicode identifiers yet, so we never have to
  // create mangling for unicode values for now. However, this is handled
  // by the v0 mangling scheme. The grammar for unicode identifier is contained
  // in <undisambiguated-identifier>, right under the <identifier> one. If the
  // identifier contains unicode values, then an extra "u" needs to be added
  // to the mangling string and `punycode` must be used to encode the
  // characters.

  mangled += std::to_string (identifier.size ());

  // If the first character of the identifier is a digit or an underscore, we
  // add an extra underscore
  if (identifier[0] == '_')
    mangled.append ("_");

  mangled.append (identifier);
}

static std::string
v0_type_prefix (const TyTy::BaseType *ty)
{
  auto ty_prefix = v0_simple_type_prefix (ty);
  if (!ty_prefix.empty ())
    return ty_prefix;

  // FIXME: We need to fetch more type prefixes
  gcc_unreachable ();
}

static std::string
legacy_mangle_item (const TyTy::BaseType *ty,
		    const Resolver::CanonicalPath &path,
		    const std::string &crate_name)
{
  const std::string hash = legacy_hash (ty->as_string ());
  const std::string hash_sig = legacy_mangle_name (hash);

  return kMangledSymbolPrefix + legacy_mangle_name (crate_name)
	 + legacy_mangle_canonical_path (path) + hash_sig + kMangledSymbolDelim;
}

// FIXME this is a wee bit broken
static std::string
legacy_mangle_impl_item (const TyTy::BaseType *self, const TyTy::BaseType *ty,
			 const std::string &name, const std::string &crate_name)
{
  const std::string hash = legacy_hash (ty->as_string ());
  const std::string hash_sig = legacy_mangle_name (hash);

  return kMangledSymbolPrefix + legacy_mangle_name (crate_name)
	 + legacy_mangle_self (self) + legacy_mangle_name (name) + hash_sig
	 + kMangledSymbolDelim;
}

static std::string
v0_mangle_item (const TyTy::BaseType *ty, const Resolver::CanonicalPath &path,
		const std::string &crate_name)
{
  std::string mangled;

  // FIXME: Add real algorithm once all pieces are implemented
  auto ty_prefix = v0_type_prefix (ty);
  v0_add_identifier (mangled, crate_name);
  v0_add_disambiguator (mangled, 62);

  gcc_unreachable ();
}

static std::string
v0_mangle_impl_item (const TyTy::BaseType *self, const TyTy::BaseType *ty,
		     const std::string &name, const std::string &crate_name)
{
  gcc_unreachable ();
}

std::string
Mangler::mangle_item (const TyTy::BaseType *ty,
		      const Resolver::CanonicalPath &path,
		      const std::string &crate_name) const
{
  switch (version)
    {
    case Mangler::MangleVersion::LEGACY:
      return legacy_mangle_item (ty, path, crate_name);
    case Mangler::MangleVersion::V0:
      return v0_mangle_item (ty, path, crate_name);
    default:
      gcc_unreachable ();
    }
}

std::string
Mangler::mangle_impl_item (const TyTy::BaseType *self, const TyTy::BaseType *ty,
			   const std::string &name,
			   const std::string &crate_name) const
{
  switch (version)
    {
    case Mangler::MangleVersion::LEGACY:
      return legacy_mangle_impl_item (self, ty, name, crate_name);
    case Mangler::MangleVersion::V0:
      return v0_mangle_impl_item (self, ty, name, crate_name);
    default:
      gcc_unreachable ();
    }
}

} // namespace Compile
} // namespace Rust
