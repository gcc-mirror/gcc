// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#include "rust-compile-base.h"
#include "rust-abi.h"
#include "rust-compile-stmt.h"
#include "rust-compile-expr.h"
#include "rust-compile-fnparam.h"
#include "rust-compile-var-decl.h"
#include "rust-compile-type.h"
#include "rust-constexpr.h"
#include "rust-diagnostics.h"
#include "rust-expr.h"	// for AST::AttrInputLiteral
#include "rust-macro.h" // for AST::MetaNameValueStr
#include "rust-hir-path-probe.h"
#include "rust-type-util.h"
#include "rust-compile-implitem.h"
#include "rust-attribute-values.h"

#include "fold-const.h"
#include "stringpool.h"
#include "attribs.h"
#include "tree.h"
#include "print-tree.h"

namespace Rust {
namespace Compile {

bool inline should_mangle_item (const tree fndecl)
{
  return lookup_attribute (Values::Attributes::NO_MANGLE,
			   DECL_ATTRIBUTES (fndecl))
	 == NULL_TREE;
}

void
HIRCompileBase::setup_fndecl (tree fndecl, bool is_main_entry_point,
			      bool is_generic_fn, HIR::Visibility &visibility,
			      const HIR::FunctionQualifiers &qualifiers,
			      const AST::AttrVec &attrs)
{
  // if its the main fn or pub visibility mark its as DECL_PUBLIC
  // please see https://github.com/Rust-GCC/gccrs/pull/137
  bool is_pub = visibility.get_vis_type () == HIR::Visibility::VisType::PUBLIC;
  if (is_main_entry_point || (is_pub && !is_generic_fn))
    {
      TREE_PUBLIC (fndecl) = 1;
    }

  // is it a const fn
  DECL_DECLARED_CONSTEXPR_P (fndecl) = qualifiers.is_const ();
  if (qualifiers.is_const ())
    {
      TREE_READONLY (fndecl) = 1;
    }

  // is it inline?
  for (const auto &attr : attrs)
    {
      bool is_inline
	= attr.get_path ().as_string () == Values::Attributes::INLINE;
      bool is_must_use
	= attr.get_path ().as_string () == Values::Attributes::MUST_USE;
      bool is_cold = attr.get_path ().as_string () == Values::Attributes::COLD;
      bool is_link_section
	= attr.get_path ().as_string () == Values::Attributes::LINK_SECTION;
      bool no_mangle
	= attr.get_path ().as_string () == Values::Attributes::NO_MANGLE;
      bool is_deprecated
	= attr.get_path ().as_string () == Values::Attributes::DEPRECATED;
      bool is_proc_macro
	= attr.get_path ().as_string () == Values::Attributes::PROC_MACRO;
      bool is_proc_macro_attribute
	= attr.get_path ().as_string ()
	  == Values::Attributes::PROC_MACRO_ATTRIBUTE;
      bool is_proc_macro_derive = attr.get_path ().as_string ()
				  == Values::Attributes::PROC_MACRO_DERIVE;

      if (is_inline)
	{
	  handle_inline_attribute_on_fndecl (fndecl, attr);
	}
      else if (is_must_use)
	{
	  handle_must_use_attribute_on_fndecl (fndecl, attr);
	}
      else if (is_cold)
	{
	  handle_cold_attribute_on_fndecl (fndecl, attr);
	}
      else if (is_link_section)
	{
	  handle_link_section_attribute_on_fndecl (fndecl, attr);
	}
      else if (is_deprecated)
	{
	  handle_deprecated_attribute_on_fndecl (fndecl, attr);
	}
      else if (no_mangle)
	{
	  handle_no_mangle_attribute_on_fndecl (fndecl, attr);
	}
      else if (is_proc_macro)
	{
	  handle_bang_proc_macro_attribute_on_fndecl (fndecl, attr);
	}
      else if (is_proc_macro_attribute)
	{
	  handle_attribute_proc_macro_attribute_on_fndecl (fndecl, attr);
	}
      else if (is_proc_macro_derive)
	{
	  handle_derive_proc_macro_attribute_on_fndecl (fndecl, attr);
	}
    }
}

static void
handle_proc_macro_common (tree fndecl, const AST::Attribute &attr)
{
  DECL_ATTRIBUTES (fndecl) = tree_cons (get_identifier ("gccrs_proc_macro"),
					NULL, DECL_ATTRIBUTES (fndecl));
}

void
HIRCompileBase::handle_bang_proc_macro_attribute_on_fndecl (
  tree fndecl, const AST::Attribute &attr)
{
  handle_proc_macro_common (fndecl, attr);
  ctx->collect_bang_proc_macro (fndecl);
}

void
HIRCompileBase::handle_attribute_proc_macro_attribute_on_fndecl (
  tree fndecl, const AST::Attribute &attr)
{
  handle_proc_macro_common (fndecl, attr);
  ctx->collect_attribute_proc_macro (fndecl);
}

static std::vector<std::string>
get_attributes (const AST::Attribute &attr)
{
  std::vector<std::string> result;

  rust_assert (attr.get_attr_input ().get_attr_input_type ()
	       == Rust::AST::AttrInput::TOKEN_TREE);
  const auto &tt
    = static_cast<const AST::DelimTokenTree &> (attr.get_attr_input ());

  // TODO: Should we rely on fixed index ? Should we search for the
  // attribute tokentree instead ?

  // Derive proc macros have the following format:
  // #[proc_macro_derive(TraitName, attributes(attr1, attr2, attr3))]
  //                    -~~~~~~~~ - ~~~~~~~~~~---------------------
  //                    ^0  ^1    ^2     ^3           ^4
  // - "attributes" is stored at position 3 in the token tree
  // - attribute are stored in the delimited token tree in position 4
  constexpr size_t attr_kw_pos = 3;
  constexpr size_t attribute_list_pos = 4;

  if (tt.get_token_trees ().size () > attr_kw_pos)
    {
      rust_assert (tt.get_token_trees ()[attr_kw_pos]->as_string ()
		   == "attributes");

      auto attributes = static_cast<const AST::DelimTokenTree *> (
	tt.get_token_trees ()[attribute_list_pos].get ());

      auto &token_trees = attributes->get_token_trees ();

      for (auto i = token_trees.cbegin () + 1; // Skip opening parenthesis
	   i < token_trees.cend ();
	   i += 2) // Skip comma and closing parenthesis
	{
	  result.push_back ((*i)->as_string ());
	}
    }
  return result;
}

static std::string
get_trait_name (const AST::Attribute &attr)
{
  // Derive proc macros have the following format:
  // #[proc_macro_derive(TraitName, attributes(attr1, attr2, attr3))]
  //                    -~~~~~~~~ - ~~~~~~~~~~---------------------
  //                    ^0  ^1    ^2     ^3           ^4
  // - The trait name is stored at position 1
  constexpr size_t trait_name_pos = 1;

  rust_assert (attr.get_attr_input ().get_attr_input_type ()
	       == Rust::AST::AttrInput::TOKEN_TREE);
  const auto &tt
    = static_cast<const AST::DelimTokenTree &> (attr.get_attr_input ());
  return tt.get_token_trees ()[trait_name_pos]->as_string ();
}

void
HIRCompileBase::handle_derive_proc_macro_attribute_on_fndecl (
  tree fndecl, const AST::Attribute &attr)
{
  handle_proc_macro_common (fndecl, attr);

  attr.get_attr_input ().parse_to_meta_item ();
  CustomDeriveInfo macro
    = {fndecl, get_trait_name (attr), get_attributes (attr)};
  ctx->collect_derive_proc_macro (macro);
}

void
HIRCompileBase::handle_cold_attribute_on_fndecl (tree fndecl,
						 const AST::Attribute &attr)
{
  // simple #[cold]
  if (!attr.has_attr_input ())
    {
      tree cold = get_identifier (Values::Attributes::COLD);
      // this will get handled by the GCC backend later
      DECL_ATTRIBUTES (fndecl)
	= tree_cons (cold, NULL_TREE, DECL_ATTRIBUTES (fndecl));
      return;
    }

  rust_error_at (attr.get_locus (),
		 "attribute %<cold%> does not accept any arguments");
}

void
HIRCompileBase::handle_link_section_attribute_on_fndecl (
  tree fndecl, const AST::Attribute &attr)
{
  if (!attr.has_attr_input ())
    {
      rust_error_at (attr.get_locus (),
		     "%<link_section%> expects exactly one argment");
      return;
    }

  rust_assert (attr.get_attr_input ().get_attr_input_type ()
	       == AST::AttrInput::AttrInputType::LITERAL);

  auto &literal = static_cast<AST::AttrInputLiteral &> (attr.get_attr_input ());
  const auto &msg_str = literal.get_literal ().as_string ();

  if (decl_section_name (fndecl))
    {
      rust_warning_at (attr.get_locus (), 0, "section name redefined");
    }

  set_decl_section_name (fndecl, msg_str.c_str ());
}

void
HIRCompileBase::handle_no_mangle_attribute_on_fndecl (
  tree fndecl, const AST::Attribute &attr)
{
  if (attr.has_attr_input ())
    {
      rust_error_at (attr.get_locus (),
		     "attribute %<no_mangle%> does not accept any arguments");
      return;
    }

  DECL_ATTRIBUTES (fndecl)
    = tree_cons (get_identifier (Values::Attributes::NO_MANGLE), NULL_TREE,
		 DECL_ATTRIBUTES (fndecl));
}

void
HIRCompileBase::handle_deprecated_attribute_on_fndecl (
  tree fndecl, const AST::Attribute &attr)
{
  tree value = NULL_TREE;
  TREE_DEPRECATED (fndecl) = 1;

  // simple #[deprecated]
  if (!attr.has_attr_input ())
    return;

  const AST::AttrInput &input = attr.get_attr_input ();
  auto input_type = input.get_attr_input_type ();

  if (input_type == AST::AttrInput::AttrInputType::LITERAL)
    {
      // handle #[deprecated = "message"]
      auto &literal
	= static_cast<AST::AttrInputLiteral &> (attr.get_attr_input ());
      const auto &msg_str = literal.get_literal ().as_string ();
      value = build_string (msg_str.size (), msg_str.c_str ());
    }
  else if (input_type == AST::AttrInput::AttrInputType::TOKEN_TREE)
    {
      // handle #[deprecated(since = "...", note = "...")]
      const auto &option = static_cast<const AST::DelimTokenTree &> (input);
      AST::AttrInputMetaItemContainer *meta_item = option.parse_to_meta_item ();
      for (const auto &item : meta_item->get_items ())
	{
	  auto converted_item = item->to_meta_name_value_str ();
	  if (!converted_item)
	    continue;
	  auto key_value = converted_item->get_name_value_pair ();
	  if (key_value.first.as_string ().compare ("since") == 0)
	    {
	      // valid, but this is handled by Cargo and some third-party
	      // audit tools
	      continue;
	    }
	  else if (key_value.first.as_string ().compare ("note") == 0)
	    {
	      const auto &msg_str = key_value.second;
	      if (value)
		rust_error_at (attr.get_locus (), "multiple %<note%> items");
	      value = build_string (msg_str.size (), msg_str.c_str ());
	    }
	  else
	    {
	      rust_error_at (attr.get_locus (), ErrorCode::E0541,
			     "unknown meta item %qs",
			     key_value.first.as_string ().c_str ());
	    }
	}
    }

  if (value)
    {
      tree attr_list = build_tree_list (NULL_TREE, value);
      DECL_ATTRIBUTES (fndecl)
	= tree_cons (get_identifier (Values::Attributes::DEPRECATED), attr_list,
		     DECL_ATTRIBUTES (fndecl));
    }
}

void
HIRCompileBase::handle_inline_attribute_on_fndecl (tree fndecl,
						   const AST::Attribute &attr)
{
  // simple #[inline]
  if (!attr.has_attr_input ())
    {
      DECL_DECLARED_INLINE_P (fndecl) = 1;
      return;
    }

  const AST::AttrInput &input = attr.get_attr_input ();
  bool is_token_tree
    = input.get_attr_input_type () == AST::AttrInput::AttrInputType::TOKEN_TREE;
  rust_assert (is_token_tree);
  const auto &option = static_cast<const AST::DelimTokenTree &> (input);
  AST::AttrInputMetaItemContainer *meta_item = option.parse_to_meta_item ();
  if (meta_item->get_items ().size () != 1)
    {
      rich_location rich_locus (line_table, attr.get_locus ());
      rich_locus.add_fixit_replace ("expected one argument");
      rust_error_at (rich_locus, ErrorCode::E0534,
		     "invalid number of arguments");
      return;
    }

  const std::string inline_option
    = meta_item->get_items ().at (0)->as_string ();

  // we only care about NEVER and ALWAYS else its an error
  bool is_always = inline_option.compare ("always") == 0;
  bool is_never = inline_option.compare ("never") == 0;

  // #[inline(never)]
  if (is_never)
    {
      DECL_UNINLINABLE (fndecl) = 1;
    }
  // #[inline(always)]
  else if (is_always)
    {
      DECL_DECLARED_INLINE_P (fndecl) = 1;
      DECL_ATTRIBUTES (fndecl) = tree_cons (get_identifier ("always_inline"),
					    NULL, DECL_ATTRIBUTES (fndecl));
    }
  else
    {
      rich_location rich_locus (line_table, attr.get_locus ());
      rich_locus.add_fixit_replace ("unknown inline option");
      rust_error_at (rich_locus, ErrorCode::E0535,
		     "invalid argument, %<inline%> attribute only accepts "
		     "%<always%> or %<never%>");
    }
}

void
HIRCompileBase::handle_must_use_attribute_on_fndecl (tree fndecl,
						     const AST::Attribute &attr)
{
  tree nodiscard = get_identifier ("nodiscard");
  tree value = NULL_TREE;

  if (attr.has_attr_input ())
    {
      rust_assert (attr.get_attr_input ().get_attr_input_type ()
		   == AST::AttrInput::AttrInputType::LITERAL);

      auto &literal
	= static_cast<AST::AttrInputLiteral &> (attr.get_attr_input ());
      const auto &msg_str = literal.get_literal ().as_string ();
      tree message = build_string (msg_str.size (), msg_str.c_str ());

      value = tree_cons (nodiscard, message, NULL_TREE);
    }

  DECL_ATTRIBUTES (fndecl)
    = tree_cons (nodiscard, value, DECL_ATTRIBUTES (fndecl));
}

void
HIRCompileBase::setup_abi_options (tree fndecl, ABI abi)
{
  tree abi_tree = NULL_TREE;

  switch (abi)
    {
    case Rust::ABI::RUST:
    case Rust::ABI::INTRINSIC:
    case Rust::ABI::C:
    case Rust::ABI::CDECL:
      // `decl_attributes` function (not the macro) has the side-effect of
      // actually switching the codegen backend to use the ABI we annotated.
      // However, since `cdecl` is the default ABI GCC will be using,
      // explicitly specifying that ABI will cause GCC to emit a warning
      // saying the attribute is useless (which is confusing to the user as
      // the attribute is added by us).
      DECL_ATTRIBUTES (fndecl)
	= tree_cons (get_identifier ("cdecl"), NULL, DECL_ATTRIBUTES (fndecl));

      return;

    case Rust::ABI::STDCALL:
      abi_tree = get_identifier ("stdcall");

      break;

    case Rust::ABI::FASTCALL:
      abi_tree = get_identifier ("fastcall");

      break;

    case Rust::ABI::SYSV64:
      abi_tree = get_identifier ("sysv_abi");

      break;

    case Rust::ABI::WIN_64:
      abi_tree = get_identifier ("ms_abi");

      break;

    default:
      break;
    }

  decl_attributes (&fndecl, build_tree_list (abi_tree, NULL_TREE), 0);
}

// ported from gcc/c/c-typecheck.c
//
// Mark EXP saying that we need to be able to take the
// address of it; it should not be allocated in a register.
// Returns true if successful.  ARRAY_REF_P is true if this
// is for ARRAY_REF construction - in that case we don't want
// to look through VIEW_CONVERT_EXPR from VECTOR_TYPE to ARRAY_TYPE,
// it is fine to use ARRAY_REFs for vector subscripts on vector
// register variables.
bool
HIRCompileBase::mark_addressable (tree exp, location_t locus)
{
  tree x = exp;

  while (1)
    switch (TREE_CODE (x))
      {
      case VIEW_CONVERT_EXPR:
	if (TREE_CODE (TREE_TYPE (x)) == ARRAY_TYPE
	    && VECTOR_TYPE_P (TREE_TYPE (TREE_OPERAND (x, 0))))
	  return true;
	x = TREE_OPERAND (x, 0);
	break;

      case COMPONENT_REF:
	// TODO
	// if (DECL_C_BIT_FIELD (TREE_OPERAND (x, 1)))
	//   {
	//     error ("cannot take address of bit-field %qD", TREE_OPERAND (x,
	//     1)); return false;
	//   }

	/* FALLTHRU */
      case ADDR_EXPR:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	x = TREE_OPERAND (x, 0);
	break;

      case COMPOUND_LITERAL_EXPR:
	TREE_ADDRESSABLE (x) = 1;
	TREE_ADDRESSABLE (COMPOUND_LITERAL_EXPR_DECL (x)) = 1;
	return true;

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return true;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	// (we don't have a concept of a "register" declaration)
	// fallthrough */

	/* FALLTHRU */
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;

	/* FALLTHRU */
      default:
	return true;
      }

  return false;
}

tree
HIRCompileBase::address_expression (tree expr, location_t location)
{
  if (expr == error_mark_node)
    return error_mark_node;

  if (!mark_addressable (expr, location))
    return error_mark_node;

  return build_fold_addr_expr_loc (location, expr);
}

tree
HIRCompileBase::indirect_expression (tree expr, location_t locus)
{
  if (expr == error_mark_node)
    return error_mark_node;

  return build_fold_indirect_ref_loc (locus, expr);
}

void
HIRCompileBase::compile_function_body (tree fndecl,
				       HIR::BlockExpr &function_body,
				       TyTy::BaseType *fn_return_ty)
{
  for (auto &s : function_body.get_statements ())
    {
      auto compiled_expr = CompileStmt::Compile (s.get (), ctx);
      if (compiled_expr != nullptr)
	{
	  tree s = convert_to_void (compiled_expr, ICV_STATEMENT);
	  ctx->add_statement (s);
	}
    }

  if (function_body.has_expr ())
    {
      location_t locus = function_body.get_final_expr ()->get_locus ();
      tree return_value = CompileExpr::Compile (function_body.expr.get (), ctx);

      // we can only return this if non unit value return type
      if (!fn_return_ty->is_unit ())
	{
	  HirId id = function_body.get_mappings ().get_hirid ();
	  location_t lvalue_locus = function_body.get_locus ();
	  location_t rvalue_locus = locus;

	  TyTy::BaseType *expected = fn_return_ty;
	  TyTy::BaseType *actual = nullptr;
	  bool ok = ctx->get_tyctx ()->lookup_type (
	    function_body.expr->get_mappings ().get_hirid (), &actual);
	  rust_assert (ok);

	  return_value = coercion_site (id, return_value, actual, expected,
					lvalue_locus, rvalue_locus);

	  tree return_stmt
	    = Backend::return_statement (fndecl, return_value, locus);
	  ctx->add_statement (return_stmt);
	}
      else
	{
	  // just add the stmt expression
	  ctx->add_statement (return_value);

	  // now just return unit expression
	  tree unit_expr = unit_expression (ctx, locus);
	  tree return_stmt
	    = Backend::return_statement (fndecl, unit_expr, locus);
	  ctx->add_statement (return_stmt);
	}
    }
  else if (fn_return_ty->is_unit ())
    {
      // we can only do this if the function is of unit type otherwise other
      // errors should have occurred
      location_t locus = function_body.get_locus ();
      tree return_value = unit_expression (ctx, locus);
      tree return_stmt
	= Backend::return_statement (fndecl, return_value, locus);
      ctx->add_statement (return_stmt);
    }
}

static ABI
get_abi (const AST::AttrVec &outer_attrs,
	 const HIR::FunctionQualifiers &qualifiers)
{
  bool is_proc_macro = std::any_of (outer_attrs.cbegin (), outer_attrs.cend (),
				    [] (const AST::Attribute &attr) {
				      auto path = attr.get_path ().as_string ();
				      return path == "proc_macro"
					     || path == "proc_macro_derive"
					     || path == "proc_macro_attribute";
				    });

  return is_proc_macro ? ABI::CDECL : qualifiers.get_abi ();
}

tree
HIRCompileBase::compile_function (
  const std::string &fn_name, HIR::SelfParam &self_param,
  std::vector<HIR::FunctionParam> &function_params,
  const HIR::FunctionQualifiers &qualifiers, HIR::Visibility &visibility,
  AST::AttrVec &outer_attrs, location_t locus, HIR::BlockExpr *function_body,
  const Resolver::CanonicalPath *canonical_path, TyTy::FnType *fntype)
{
  tree compiled_fn_type = TyTyResolveCompile::compile (ctx, fntype);
  std::string ir_symbol_name
    = canonical_path->get () + fntype->subst_as_string ();

  // we don't mangle the main fn since we haven't implemented the main shim
  bool is_main_fn = fn_name.compare ("main") == 0;
  std::string asm_name = fn_name;

  unsigned int flags = 0;
  tree fndecl = Backend::function (compiled_fn_type, ir_symbol_name,
				   "" /* asm_name */, flags, locus);

  setup_fndecl (fndecl, is_main_fn, fntype->has_substitutions_defined (),
		visibility, qualifiers, outer_attrs);
  setup_abi_options (fndecl, get_abi (outer_attrs, qualifiers));

  // conditionally mangle the function name
  bool should_mangle = should_mangle_item (fndecl);
  if (!is_main_fn && should_mangle)
    asm_name = ctx->mangle_item (fntype, *canonical_path);
  SET_DECL_ASSEMBLER_NAME (fndecl,
			   get_identifier_with_length (asm_name.data (),
						       asm_name.length ()));

  // insert into the context
  ctx->insert_function_decl (fntype, fndecl);

  // setup the params
  TyTy::BaseType *tyret = fntype->get_return_type ();
  std::vector<Bvariable *> param_vars;
  if (!self_param.is_error ())
    {
      rust_assert (fntype->is_method ());
      TyTy::BaseType *self_tyty_lookup = fntype->get_self_type ();

      tree self_type = TyTyResolveCompile::compile (ctx, self_tyty_lookup);
      Bvariable *compiled_self_param
	= CompileSelfParam::compile (ctx, fndecl, self_param, self_type,
				     self_param.get_locus ());

      param_vars.push_back (compiled_self_param);
      ctx->insert_var_decl (self_param.get_mappings ().get_hirid (),
			    compiled_self_param);
    }

  // offset from + 1 for the TyTy::FnType being used when this is a method to
  // skip over Self on the FnType
  bool is_method = !self_param.is_error ();
  size_t i = is_method ? 1 : 0;
  for (auto &referenced_param : function_params)
    {
      auto tyty_param = fntype->param_at (i++);
      auto param_tyty = tyty_param.second;
      auto compiled_param_type = TyTyResolveCompile::compile (ctx, param_tyty);

      location_t param_locus = referenced_param.get_locus ();
      Bvariable *compiled_param_var
	= CompileFnParam::compile (ctx, fndecl, &referenced_param,
				   compiled_param_type, param_locus);

      param_vars.push_back (compiled_param_var);

      const HIR::Pattern &param_pattern = *referenced_param.get_param_name ();
      ctx->insert_var_decl (param_pattern.get_mappings ().get_hirid (),
			    compiled_param_var);
    }

  if (!Backend::function_set_parameters (fndecl, param_vars))
    return error_mark_node;

  tree enclosing_scope = NULL_TREE;
  location_t start_location = function_body->get_locus ();
  location_t end_location = function_body->get_end_locus ();

  tree code_block = Backend::block (fndecl, enclosing_scope, {} /*locals*/,
				    start_location, end_location);
  ctx->push_block (code_block);

  Bvariable *return_address = nullptr;
  tree return_type = TyTyResolveCompile::compile (ctx, tyret);

  bool address_is_taken = false;
  tree ret_var_stmt = NULL_TREE;
  return_address
    = Backend::temporary_variable (fndecl, code_block, return_type, NULL,
				   address_is_taken, locus, &ret_var_stmt);

  ctx->add_statement (ret_var_stmt);

  ctx->push_fn (fndecl, return_address, tyret);
  compile_function_body (fndecl, *function_body, tyret);
  tree bind_tree = ctx->pop_block ();

  gcc_assert (TREE_CODE (bind_tree) == BIND_EXPR);
  DECL_SAVED_TREE (fndecl) = bind_tree;

  ctx->pop_fn ();
  ctx->push_function (fndecl);

  if (DECL_DECLARED_CONSTEXPR_P (fndecl))
    {
      maybe_save_constexpr_fundef (fndecl);
    }

  return fndecl;
}

tree
HIRCompileBase::compile_constant_item (
  TyTy::BaseType *resolved_type, const Resolver::CanonicalPath *canonical_path,
  HIR::Expr *const_value_expr, location_t locus)
{
  const std::string &ident = canonical_path->get ();

  tree type = TyTyResolveCompile::compile (ctx, resolved_type);
  tree const_type = build_qualified_type (type, TYPE_QUAL_CONST);
  bool is_block_expr
    = const_value_expr->get_expression_type () == HIR::Expr::ExprType::Block;

  // in order to compile a block expr we want to reuse as much existing
  // machineary that we already have. This means the best approach is to
  // make a _fake_ function with a block so it can hold onto temps then
  // use our constexpr code to fold it completely or error_mark_node
  Backend::typed_identifier receiver;
  tree compiled_fn_type = Backend::function_type (
    receiver, {}, {Backend::typed_identifier ("_", const_type, locus)}, NULL,
    locus);
  tree fndecl = Backend::function (compiled_fn_type, ident, "", 0, locus);
  TREE_READONLY (fndecl) = 1;

  tree enclosing_scope = NULL_TREE;
  location_t start_location = const_value_expr->get_locus ();
  location_t end_location = const_value_expr->get_locus ();
  if (is_block_expr)
    {
      HIR::BlockExpr *function_body
	= static_cast<HIR::BlockExpr *> (const_value_expr);
      start_location = function_body->get_locus ();
      end_location = function_body->get_end_locus ();
    }

  tree code_block = Backend::block (fndecl, enclosing_scope, {} /*locals*/,
				    start_location, end_location);
  ctx->push_block (code_block);

  bool address_is_taken = false;
  tree ret_var_stmt = NULL_TREE;
  Bvariable *return_address
    = Backend::temporary_variable (fndecl, code_block, const_type, NULL,
				   address_is_taken, locus, &ret_var_stmt);

  ctx->add_statement (ret_var_stmt);
  ctx->push_fn (fndecl, return_address, resolved_type);

  if (is_block_expr)
    {
      HIR::BlockExpr *function_body
	= static_cast<HIR::BlockExpr *> (const_value_expr);
      compile_function_body (fndecl, *function_body, resolved_type);
    }
  else
    {
      tree value = CompileExpr::Compile (const_value_expr, ctx);

      tree return_expr
	= Backend::return_statement (fndecl, value,
				     const_value_expr->get_locus ());
      ctx->add_statement (return_expr);
    }

  tree bind_tree = ctx->pop_block ();

  gcc_assert (TREE_CODE (bind_tree) == BIND_EXPR);
  DECL_SAVED_TREE (fndecl) = bind_tree;
  DECL_DECLARED_CONSTEXPR_P (fndecl) = 1;
  maybe_save_constexpr_fundef (fndecl);

  ctx->pop_fn ();

  // lets fold it into a call expr
  tree call = build_call_array_loc (locus, const_type, fndecl, 0, NULL);
  tree folded_expr = fold_expr (call);

  return named_constant_expression (const_type, ident, folded_expr, locus);
}

tree
HIRCompileBase::named_constant_expression (tree type_tree,
					   const std::string &name,
					   tree const_val, location_t location)
{
  if (type_tree == error_mark_node || const_val == error_mark_node)
    return error_mark_node;

  tree name_tree = get_identifier_with_length (name.data (), name.length ());
  tree decl = build_decl (location, CONST_DECL, name_tree, type_tree);
  DECL_INITIAL (decl) = const_val;
  TREE_CONSTANT (decl) = 1;
  TREE_READONLY (decl) = 1;

  rust_preserve_from_gc (decl);
  return decl;
}

tree
HIRCompileBase::resolve_method_address (TyTy::FnType *fntype,
					TyTy::BaseType *receiver,
					location_t expr_locus)
{
  rust_debug_loc (expr_locus, "resolve_method_address for %s and receiver %s",
		  fntype->debug_str ().c_str (),
		  receiver->debug_str ().c_str ());

  DefId id = fntype->get_id ();
  rust_assert (id != UNKNOWN_DEFID);

  // Now we can try and resolve the address since this might be a forward
  // declared function, generic function which has not be compiled yet or
  // its an not yet trait bound function
  HIR::Item *resolved_item = ctx->get_mappings ()->lookup_defid (id);
  if (resolved_item != nullptr)
    {
      if (!fntype->has_substitutions_defined ())
	return CompileItem::compile (resolved_item, ctx);

      return CompileItem::compile (resolved_item, ctx, fntype);
    }

  // it might be resolved to a trait item
  HIR::TraitItem *trait_item
    = ctx->get_mappings ()->lookup_trait_item_defid (id);
  HIR::Trait *trait = ctx->get_mappings ()->lookup_trait_item_mapping (
    trait_item->get_mappings ().get_hirid ());

  Resolver::TraitReference *trait_ref
    = &Resolver::TraitReference::error_node ();
  bool ok = ctx->get_tyctx ()->lookup_trait_reference (
    trait->get_mappings ().get_defid (), &trait_ref);
  rust_assert (ok);

  // the type resolver can only resolve type bounds to their trait
  // item so its up to us to figure out if this path should resolve
  // to an trait-impl-block-item or if it can be defaulted to the
  // trait-impl-item's definition
  const HIR::PathIdentSegment segment (trait_item->trait_identifier ());
  auto root = receiver->get_root ();
  auto candidates
    = Resolver::PathProbeImplTrait::Probe (root, segment, trait_ref);
  if (candidates.size () == 0)
    {
      // this means we are defaulting back to the trait_item if
      // possible
      Resolver::TraitItemReference *trait_item_ref = nullptr;
      bool ok = trait_ref->lookup_hir_trait_item (*trait_item, &trait_item_ref);
      rust_assert (ok);				    // found
      rust_assert (trait_item_ref->is_optional ()); // has definition

      // FIXME tl::optional means it has a definition and an associated
      // block which can be a default implementation, if it does not
      // contain an implementation we should actually return
      // error_mark_node

      return CompileTraitItem::Compile (trait_item_ref->get_hir_trait_item (),
					ctx, fntype, true, expr_locus);
    }

  const Resolver::PathProbeCandidate *selectedCandidate = nullptr;
  rust_debug_loc (expr_locus, "resolved to %lu candidates",
		  (unsigned long) candidates.size ());

  // filter for the possible case of non fn type items
  std::set<Resolver::PathProbeCandidate> filteredFunctionCandidates;
  for (auto &candidate : candidates)
    {
      bool is_fntype = candidate.ty->get_kind () == TyTy::TypeKind::FNDEF;
      if (!is_fntype)
	continue;

      filteredFunctionCandidates.insert (candidate);
    }

  // look for the exact fntype
  for (auto &candidate : filteredFunctionCandidates)
    {
      if (filteredFunctionCandidates.size () == 1)
	{
	  selectedCandidate = &candidate;
	  break;
	}

      bool compatable
	= Resolver::types_compatable (TyTy::TyWithLocation (candidate.ty),
				      TyTy::TyWithLocation (fntype), expr_locus,
				      false);

      rust_debug_loc (candidate.locus, "candidate: %s vs %s compatable=%s",
		      candidate.ty->debug_str ().c_str (),
		      fntype->debug_str ().c_str (),
		      compatable ? "true" : "false");

      if (compatable)
	{
	  selectedCandidate = &candidate;
	  break;
	}
    }

  // FIXME eventually this should just return error mark node when we support
  // going through all the passes
  rust_assert (selectedCandidate != nullptr);

  // lets compile it
  const Resolver::PathProbeCandidate &candidate = *selectedCandidate;
  rust_assert (candidate.is_impl_candidate ());
  rust_assert (candidate.ty->get_kind () == TyTy::TypeKind::FNDEF);
  TyTy::FnType *candidate_call = static_cast<TyTy::FnType *> (candidate.ty);
  HIR::ImplItem *impl_item = candidate.item.impl.impl_item;

  TyTy::BaseType *monomorphized = candidate_call;
  if (candidate_call->needs_generic_substitutions ())
    {
      TyTy::BaseType *infer_impl_call
	= candidate_call->infer_substitions (expr_locus);
      monomorphized
	= Resolver::unify_site (fntype->get_ref (),
				TyTy::TyWithLocation (infer_impl_call),
				TyTy::TyWithLocation (fntype), expr_locus);
    }

  return CompileInherentImplItem::Compile (impl_item, ctx, monomorphized);
}

tree
HIRCompileBase::unit_expression (Context *ctx, location_t locus)
{
  tree unit_type = TyTyResolveCompile::get_unit_type (ctx);
  return Backend::constructor_expression (unit_type, false, {}, -1, locus);
}

} // namespace Compile
} // namespace Rust
