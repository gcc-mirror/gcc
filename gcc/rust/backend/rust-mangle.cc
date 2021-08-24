#include "rust-mangle.h"
#include "fnv-hash.h"

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

// FIXME: Uncomment once v0 mangling is implemented
// static std::string
// Mangler::v0_mangle_item (const TyTy::BaseType *ty,
// 			 const std::string &name)
// {}
//
// static std::string
// Mangler::v0_mangle_impl_item (const TyTy::BaseType *self,
// 			      const TyTy::BaseType *ty,
// 			      const std::string &name)
// {}

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
      gcc_unreachable ();
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
      gcc_unreachable ();
    default:
      gcc_unreachable ();
    }
}

} // namespace Compile
} // namespace Rust
