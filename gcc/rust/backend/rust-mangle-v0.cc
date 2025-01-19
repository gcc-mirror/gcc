// Copyright (C) 2020-2025 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "rust-mangle.h"
#include "optional.h"
#include "rust-base62.h"
#include "rust-diagnostics.h"
#include "rust-system.h"
#include "rust-tyty.h"
#include "rust-unicode.h"
#include "rust-punycode.h"
#include "rust-compile-type.h"
#include <sstream>

namespace Rust {
namespace Compile {

struct V0Path
{
  std::string prefix = "";
  // Used for "N"
  std::string ns = "";
  std::string path = "";
  // Used for "N" and "C"
  std::string ident = "";
  std::string disambiguator = "";
  // Used for "M" and "X"
  std::string impl_path = "";
  std::string impl_type = "";
  std::string trait_type = "";
  // Used for generic types
  std::string generic_postfix = "";
  std::string generic_prefix = "";

  std::string as_string () const
  {
    if (prefix == "N")
      return generic_prefix + prefix + ns + path + disambiguator + ident
	     + generic_postfix;
    else if (prefix == "M")
      return prefix + impl_path + impl_type;
    else if (prefix == "X")
      return prefix + impl_type + trait_type;
    else if (prefix == "C")
      return prefix + disambiguator + ident;
    else
      rust_unreachable ();
  }
};

static std::string
v0_path (Rust::Compile::Context *ctx, const TyTy::BaseType *ty,
	 const Resolver::CanonicalPath &path);

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
    {"i8", "a"},    {"u8", "h"},    {"i16", "s"}, {"u16", "t"},
    {"i32", "l"},   {"u32", "m"},   {"i64", "x"}, {"u64", "y"},
    {"isize", "i"}, {"usize", "j"}, {"f32", "f"}, {"f64", "d"},
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

  rust_unreachable ();
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
    case TyTy::TypeKind::USIZE:
      return v0_numeric_prefix (ty);

    default:
      return "";
    }

  rust_unreachable ();
}

static std::string
v0_complex_type_prefix (Context *ctx, const TyTy::BaseType *ty)
{
  // FIXME: ref, slice, dyn, etc.
  // TODO: generics
  switch (ty->get_kind ())
    {
      case TyTy::TypeKind::ADT: {
	const TyTy::ADTType *adt = static_cast<const TyTy::ADTType *> (ty);
	return v0_path (ctx, ty, adt->get_ident ().path);
      }
      break;
    default:
      return "";
    }
}

// Returns an underscore-terminated base62 integer.
// This corresponds to the `<base-62-number>` grammar in the v0 mangling RFC:
//  - 0 is encoded as "_"
//  - any other value is encoded as itself minus one in base 62, followed by
//  "_"
static std::string
v0_integer_62 (uint64_t x)
{
  std::stringstream s;
  if (x > 0)
    s << base62_integer (x - 1);

  s << "_";
  return s.str ();
}

//  Returns a tag-prefixed base62 integer when the
// integer is greater than 0:
//  - 0 is encoded as "" (nothing)
//  - any other value is encoded as <tag> + v0_integer_62(itself), that is
//  <tag> + base62(itself - 1) + '_'
static std::string
v0_opt_integer_62 (std::string tag, uint64_t x)
{
  if (x > 0)
    {
      return tag + v0_integer_62 (x);
    }
  return "";
}

static std::string
v0_disambiguator (uint64_t dis)
{
  return v0_opt_integer_62 ("s", dis);
}

static std::string
v0_type_prefix (Context *ctx, const TyTy::BaseType *ty)
{
  std::string ty_prefix;

  ty_prefix = v0_simple_type_prefix (ty);
  if (!ty_prefix.empty ())
    return ty_prefix;

  ty_prefix = v0_complex_type_prefix (ctx, ty);
  if (!ty_prefix.empty ())
    return ty_prefix;

  rust_unreachable ();
}

static std::string
v0_generic_args (Context *ctx, const TyTy::BaseType *ty)
{
  std::stringstream ss;
  const TyTy::FnType *fnty = static_cast<const TyTy::FnType *> (ty);
  TyTy::SubstitutionArgumentMappings &subst_ref
    = const_cast<TyTy::FnType *> (fnty)->get_substitution_arguments ();
  for (TyTy::SubstitutionArg &map : subst_ref.get_mappings ())
    {
      ss << v0_type_prefix (ctx, map.get_tyty ());
    }
  return ss.str ();
}

// Returns an mangled identifier. This corresponds to the
// `<identifier>` grammar in the v0 mangling RFC.
static std::string
v0_identifier (const std::string &identifier)
{
  std::stringstream mangled;
  // The grammar for unicode identifier is contained in
  // <undisambiguated-identifier>, right under the <identifier> one. If the
  // identifier contains unicode values, then an extra "u" needs to be added to
  // the mangling string and `punycode` must be used to encode the characters.

  if (!is_ascii_only (identifier))
    mangled << "u";

  tl::optional<Utf8String> uident_opt
    = Utf8String::make_utf8_string (identifier);
  rust_assert (uident_opt.has_value ());
  tl::optional<std::string> punycode_opt
    = encode_punycode (uident_opt.value ());
  rust_assert (punycode_opt.has_value ());

  std::string punycode = punycode_opt.value ();

  // remove a tailing hyphen
  if (punycode.back () == '-')
    punycode.pop_back ();

  // replace a hyphen in punycode with a underscore
  std::replace (punycode.begin (), punycode.end (), '-', '_');

  mangled << std::to_string (punycode.size ());

  // Add extra '_'
  if (punycode[0] == '_' || ('0' <= punycode[0] && punycode[0] <= '9'))
    mangled << "_";

  mangled << punycode;
  return mangled.str ();
}

static V0Path
v0_type_path (V0Path path, std::string ident)
{
  V0Path v0path;
  v0path.prefix = "N";
  v0path.ns = "t";
  v0path.path = path.as_string ();
  v0path.ident = ident;
  // TODO: Need <generic-arg>?
  return v0path;
}

static V0Path
v0_function_path (V0Path path, Rust::Compile::Context *ctx,
		  const TyTy::BaseType *ty, HIR::Function *fn,
		  std::string ident)
{
  V0Path v0path;
  v0path.prefix = "N";
  v0path.ns = "v";
  v0path.path = path.as_string ();
  v0path.ident = ident;
  if (!fn->get_generic_params ().empty ())
    {
      v0path.generic_prefix = "I";
      v0path.generic_postfix = v0_generic_args (ctx, ty) + "E";
    }
  return v0path;
}

static V0Path
v0_scope_path (V0Path path, std::string ident, std::string ns)
{
  V0Path v0path;
  v0path.prefix = "N";
  v0path.ns = ns;
  v0path.path = path.as_string ();
  v0path.ident = ident;
  return v0path;
}

static V0Path
v0_crate_path (CrateNum crate_num, std::string ident)
{
  V0Path v0path;
  v0path.prefix = "C";
  v0path.disambiguator = v0_disambiguator (crate_num);
  v0path.ident = ident;
  return v0path;
}

static V0Path
v0_inherent_or_trait_impl_path (Rust::Compile::Context *ctx,
				HIR::ImplBlock *impl_block)
{
  V0Path v0path;
  bool ok;

  // lookup impl type
  TyTy::BaseType *impl_ty = nullptr;
  ok = ctx->get_tyctx ()->lookup_type (
    impl_block->get_type ()->get_mappings ().get_hirid (), &impl_ty);
  rust_assert (ok);

  // FIXME: dummy value for now
  v0path.impl_path = "C5crate";
  v0path.impl_type = v0_type_prefix (ctx, impl_ty);

  if (impl_block->has_trait_ref ())
    {
      // trait impl: X <impl-path> <type> <path>
      v0path.prefix = "X";

      TyTy::BaseType *trait_ty = nullptr;
      ok = ctx->get_tyctx ()->lookup_type (
	impl_block->get_trait_ref ()->get_mappings ().get_hirid (), &trait_ty);
      rust_assert (ok);

      v0path.trait_type = v0_type_prefix (ctx, trait_ty);
    }
  else
    // inherent impl: M <impl-path> <type>
    v0path.prefix = "M";

  return v0path;
}

static V0Path
v0_closure (V0Path path, HirId closure)
{
  V0Path v0path;
  v0path.prefix = "N";
  v0path.ns = "C";
  v0path.disambiguator = v0_disambiguator (closure);
  v0path.path = path.as_string ();
  v0path.ident = "0";
  return v0path;
}

static std::string
v0_path (Rust::Compile::Context *ctx, const TyTy::BaseType *ty,
	 const Resolver::CanonicalPath &cpath)
{
  auto mappings = Analysis::Mappings::get ();

  V0Path v0path = {};

  cpath.iterate_segs ([&] (const Resolver::CanonicalPath &seg) {
    HirId hir_id;
    bool ok = mappings->lookup_node_to_hir (seg.get_node_id (), &hir_id);
    if (!ok)
      {
	// FIXME: generic arg in canonical path? (e.g. <i32> in crate::S<i32>)
	rust_unreachable ();
      }

    HirId parent_impl_id = UNKNOWN_HIRID;
    HIR::ImplItem *impl_item
      = mappings->lookup_hir_implitem (hir_id, &parent_impl_id);
    HIR::TraitItem *trait_item = mappings->lookup_hir_trait_item (hir_id);
    HIR::Item *item = mappings->lookup_hir_item (hir_id);
    HIR::Expr *expr = mappings->lookup_hir_expr (hir_id);

    if (impl_item != nullptr)
      {
	switch (impl_item->get_impl_item_type ())
	  {
	    case HIR::ImplItem::FUNCTION: {
	      HIR::Function *fn = static_cast<HIR::Function *> (impl_item);
	      v0path = v0_function_path (v0path, ctx, ty, fn,
					 v0_identifier (seg.get ()));
	    }
	    break;
	  case HIR::ImplItem::CONSTANT:
	    v0path = v0_scope_path (v0path, v0_identifier (seg.get ()), "v");
	    break;
	  default:
	    rust_internal_error_at (UNDEF_LOCATION, "Attempt to mangle '%s'",
				    cpath.get ().c_str ());
	    break;
	  }
      }
    else if (trait_item != nullptr)
      {
	switch (trait_item->get_item_kind ())
	  {
	    case HIR::TraitItem::FUNC: {
	      HIR::Function *fn = static_cast<HIR::Function *> (impl_item);
	      v0path = v0_function_path (v0path, ctx, ty, fn,
					 v0_identifier (seg.get ()));
	    }
	    break;
	  case HIR::TraitItem::CONST:
	    v0path = v0_scope_path (v0path, v0_identifier (seg.get ()), "v");
	    break;
	  default:
	    rust_internal_error_at (UNDEF_LOCATION, "Attempt to mangle '%s'",
				    cpath.get ().c_str ());
	    break;
	  }
      }
    else if (item != nullptr)
      switch (item->get_item_kind ())
	{
	  case HIR::Item::ItemKind::Function: {
	    HIR::Function *fn = static_cast<HIR::Function *> (item);
	    v0path = v0_function_path (v0path, ctx, ty, fn,
				       v0_identifier (seg.get ()));
	  }
	  break;
	case HIR::Item::ItemKind::Module:
	  v0path = v0_scope_path (v0path, v0_identifier (seg.get ()), "t");
	  break;
	case HIR::Item::ItemKind::Trait: // FIXME: correct?
	case HIR::Item::ItemKind::Static:
	case HIR::Item::ItemKind::Constant:
	  v0path = v0_scope_path (v0path, v0_identifier (seg.get ()), "v");
	  break;
	case HIR::Item::ItemKind::Struct:
	case HIR::Item::ItemKind::Enum:
	case HIR::Item::ItemKind::Union:
	  v0path = v0_type_path (v0path, v0_identifier (seg.get ()));
	  break;
	case HIR::Item::ItemKind::Impl:
	  // Trait impl or inherent impl.
	  {
	    HIR::ImplBlock *impl_block = static_cast<HIR::ImplBlock *> (item);
	    v0path = v0_inherent_or_trait_impl_path (ctx, impl_block);
	  }
	  break;
	case HIR::Item::ItemKind::ExternBlock:
	case HIR::Item::ItemKind::ExternCrate:
	case HIR::Item::ItemKind::UseDeclaration:
	case HIR::Item::ItemKind::TypeAlias:
	case HIR::Item::ItemKind::EnumItem: // FIXME: correct?
	  rust_internal_error_at (UNDEF_LOCATION, "Attempt to mangle '%s'",
				  cpath.get ().c_str ());
	  break;
	}
    else if (expr != nullptr)
      {
	rust_assert (expr->get_expression_type ()
		     == HIR::Expr::ExprType::Closure);
	// Use HIR ID as disambiguator.
	v0path = v0_closure (v0path, hir_id);
      }
    else
      // Not HIR item, impl item, trait impl item, nor expr. Assume a crate.
      v0path
	= v0_crate_path (cpath.get_crate_num (), v0_identifier (seg.get ()));

    return true;
  });

  return v0path.as_string ();
}

std::string
v0_mangle_item (Rust::Compile::Context *ctx, const TyTy::BaseType *ty,
		const Resolver::CanonicalPath &path)
{
  rust_debug ("Start mangling: %s", path.get ().c_str ());

  // TODO: get Instanciating CrateNum
  // auto mappings = Analysis::Mappings::get ();
  // std::string crate_name;
  // bool ok = mappings->get_crate_name (path.get_crate_num (), crate_name);
  // rust_assert (ok);

  std::stringstream mangled;
  mangled << "_R";
  mangled << v0_path (ctx, ty, path);

  rust_debug ("=> %s", mangled.str ().c_str ());

  return mangled.str ();
}

} // namespace Compile
} // namespace Rust
