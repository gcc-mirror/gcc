// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

#ifndef RUST_COMPILE_EXTERN_ITEM
#define RUST_COMPILE_EXTERN_ITEM

#include "rust-compile-base.h"
#include "rust-compile-intrinsic.h"
#include "rust-compile-type.h"
#include "rust-diagnostics.h"
#include "rust-hir-full-decls.h"
#include "rust-attributes.h"
#include "rust-attribute-values.h"
#include "rust-builtins.h"
#include "rust-compile-fnparam.h"
#include "fold-const.h"

namespace Rust {
namespace Compile {

class CompileExternItem : public HIRCompileBase,
			  public HIR::HIRExternalItemVisitor
{
public:
  static tree compile (HIR::ExternalItem *item, Context *ctx,
		       TyTy::BaseType *concrete = nullptr,
		       location_t ref_locus = UNDEF_LOCATION)
  {
    CompileExternItem compiler (ctx, concrete, ref_locus);
    item->accept_vis (compiler);
    return compiler.reference;
  }

  void visit (HIR::ExternalStaticItem &item) override
  {
    // check if its already been compiled
    Bvariable *lookup = Bvariable::error_variable ();
    if (ctx->lookup_var_decl (item.get_mappings ().get_hirid (), &lookup))
      {
	reference = Backend::var_expression (lookup, ref_locus);
	return;
      }

    TyTy::BaseType *resolved_type = nullptr;
    bool ok = ctx->get_tyctx ()->lookup_type (item.get_mappings ().get_hirid (),
					      &resolved_type);
    rust_assert (ok);

    std::string name = item.get_item_name ().as_string ();
    GGC::Ident asm_name = get_link_name (item);

    tree type = TyTyResolveCompile::compile (ctx, resolved_type);
    bool is_external = true;
    bool is_hidden = false;
    bool in_unique_section = false;

    Bvariable *static_global
      = Backend::global_variable (name, asm_name, type, is_external, is_hidden,
				  in_unique_section, item.get_locus ());
    ctx->insert_var_decl (item.get_mappings ().get_hirid (), static_global);
    ctx->push_var (static_global);

    reference = Backend::var_expression (static_global, ref_locus);
  }

  void visit (HIR::ExternalFunctionItem &function) override
  {
    TyTy::BaseType *fntype_tyty;
    if (!ctx->get_tyctx ()->lookup_type (function.get_mappings ().get_hirid (),
					 &fntype_tyty))
      {
	rust_fatal_error (function.get_locus (),
			  "failed to lookup function type");
	return;
      }

    rust_assert (fntype_tyty->get_kind () == TyTy::TypeKind::FNDEF);
    TyTy::FnType *fntype = static_cast<TyTy::FnType *> (fntype_tyty);
    if (fntype->has_substitutions_defined ())
      {
	// we cant do anything for this only when it is used and a concrete type
	// is given
	if (concrete == nullptr)
	  return;
	else
	  {
	    rust_assert (concrete->get_kind () == TyTy::TypeKind::FNDEF);
	    fntype = static_cast<TyTy::FnType *> (concrete);
	  }
      }

    // items can be forward compiled which means we may not need to invoke this
    // code. We might also have already compiled this generic function as well.
    tree lookup = NULL_TREE;
    if (ctx->lookup_function_decl (fntype->get_ty_ref (), &lookup,
				   fntype->get_id (), fntype))
      {
	reference = address_expression (lookup, ref_locus);
	return;
      }

    if (fntype->has_substitutions_defined ())
      // override the HIR lookups for the substitutions in this context
      fntype->override_context ();

    if (fntype->get_abi () == ABI::INTRINSIC)
      {
	Intrinsics compile (ctx);
	tree fndecl = compile.compile (fntype, ref_locus);
	ctx->insert_function_decl (fntype, fndecl);
	return;
      }

    tree compiled_fn_type = TyTyResolveCompile::compile (ctx, fntype);
    std::string ir_symbol_name = function.get_item_name ().as_string ();
    GGC::Ident asm_name = get_link_name (function);

    if (fntype->get_abi () == ABI::UNADJUSTED)
      {
	tree resolved;
	LlvmBuiltinAdapter adapter;
	auto mapping_res = BuiltinsContext::get ().map_llvm_to_gcc_builtin (
	  asm_name.as_string (), &resolved, &adapter);

	switch (mapping_res)
	  {
	  case LlvmBuiltinMappingResult::NOT_MAPPED:
	    rust_error_at (function.get_locus (),
			   "LLVM intrinsic %qs is not supported at the moment",
			   asm_name.c_str ());
	    reference = error_mark_node;
	    return;
	  case LlvmBuiltinMappingResult::TARGET_UNAVAILABLE:
	    rust_error_at (
	      function.get_locus (),
	      "LLVM intrinsic %qs is not available for this target",
	      asm_name.c_str ());
	    reference = error_mark_node;
	    return;
	  case LlvmBuiltinMappingResult::RESOLVED:
	    break;
	  }

	tree adapter_tree = error_mark_node;

	switch (adapter)
	  {
	  case LlvmBuiltinAdapter::OUTPUT_POINTER_VALUE_STATUS:
	    adapter_tree = compile_x86_output_pointer_adapter (
	      ctx, fntype, resolved, OutputTupleOrder::VALUE_STATUS,
	      function.get_locus ());
	    break;
	  case LlvmBuiltinAdapter::OUTPUT_POINTER_STATUS_VALUE:
	    adapter_tree = compile_x86_output_pointer_adapter (
	      ctx, fntype, resolved, OutputTupleOrder::STATUS_VALUE,
	      function.get_locus ());
	    break;
	  case LlvmBuiltinAdapter::FORWARD_ARGUMENTS:
	    // TODO placeholder
	    adapter_tree = compile_x86_output_pointer_adapter (
	      ctx, fntype, resolved, OutputTupleOrder::STATUS_VALUE,
	      function.get_locus ());
	    break;
	  }

	if (adapter_tree == error_mark_node)
	  {
	    rust_error_at (function.get_locus (),
			   "Invalid signature for LLVM intrinsic %qs",
			   asm_name.c_str ());
	    reference = error_mark_node;
	    return;
	  }

	ctx->insert_function_decl (fntype, adapter_tree);
	reference = address_expression (adapter_tree, ref_locus);
	return;
      }

    const unsigned int flags = Backend::function_is_declaration;
    tree fndecl = Backend::function (compiled_fn_type, ir_symbol_name, asm_name,
				     flags, function.get_locus ());
    TREE_PUBLIC (fndecl) = 1;
    setup_abi_options (fndecl, fntype->get_abi ());

    ctx->insert_function_decl (fntype, fndecl);

    reference = address_expression (fndecl, ref_locus);
  }

  void visit (HIR::ExternalTypeItem &type) override
  {
    rust_sorry_at (type.get_locus (), "extern types are not supported yet");
  }

private:
  enum class OutputTupleOrder
  {
    VALUE_STATUS,
    STATUS_VALUE,
  };

  CompileExternItem (Context *ctx, TyTy::BaseType *concrete,
		     location_t ref_locus)
    : HIRCompileBase (ctx), concrete (concrete), reference (error_mark_node),
      ref_locus (ref_locus)
  {}

  template <typename T> static GGC::Ident get_link_name (T &obj)
  {
    AST::Attribute *use_attr = nullptr;

    for (auto &attr : obj.get_outer_attrs ())
      {
	if (attr.get_path ().as_string () == Values::Attributes::LINK_NAME)
	  {
	    // later attributes override earlier ones
	    // TODO: add warning -- should duplicate
	    //       attributes be folded elsewhere?
	    use_attr = &attr;
	  }
      }

    if (use_attr)
      {
	auto link_name
	  = Analysis::Attributes::extract_string_literal (*use_attr);

	if (!link_name.has_value ())
	  rust_error_at (use_attr->get_locus (),
			 "malformed %<link_name%> attribute input");
	else
	  return *link_name;
      }

    return obj.get_item_name ();
  }

  /**
   * Compiles a wrapper that wraps around GCC built-ins for compatibility
   * with LLVM built-ins function signatures returning a tuple with value +
   * status.
   *
   * @param ctx
   * @param fntype the LLVM built-in function type
   * @param gcc_builtin the GCC built-in function
   * @param order whether the LLVM built-in returns (value, status) or (status,
   * value)
   * @param locus
   * @return tree
   */
  static tree compile_x86_output_pointer_adapter (Context *ctx,
						  TyTy::FnType *fntype,
						  tree gcc_builtin,
						  OutputTupleOrder order,
						  location_t locus)
  {
    // expect to be a 2-field tuple return type
    TyTy::TupleType *tuple
      = fntype->get_return_type ()->try_as<TyTy::TupleType> ();

    if (tuple == nullptr || tuple->num_fields () != 2)
      // TODO probably add error here
      return error_mark_node;

    tree compiled_fn_type = TyTyResolveCompile::compile (ctx, fntype);

    const auto &path = fntype->get_ident ().path;
    std::string ir_name = path.get () + fntype->subst_as_string ();
    std::string asm_name = ctx->mangle_item (fntype, path);

    // start building the wrapper function
    tree fndecl
      = Backend::function (compiled_fn_type, ir_name, asm_name, 0, locus);

    TREE_PUBLIC (fndecl) = 0;
    DECL_ARTIFICIAL (fndecl) = 1;
    DECL_EXTERNAL (fndecl) = 0;
    DECL_DECLARED_INLINE_P (fndecl) = 1;

    // compile params for the rust wrapper
    std::vector<Bvariable *> param_vars;
    param_vars.reserve (fntype->get_params ().size ());
    for (auto &param : fntype->get_params ())
      {
	auto &pattern = param.get_pattern ();
	tree type = TyTyResolveCompile::compile (ctx, param.get_type ());
	Bvariable *variable
	  = CompileFnParam::compile (ctx, fndecl, pattern, type,
				     pattern.get_locus ());
	param_vars.emplace_back (variable);
      }

    if (!Backend::function_set_parameters (fndecl, param_vars))
      return error_mark_node;

    // forward the rust params, convert each into the corresponding gcc
    // built-in param type
    std::vector<tree> call_arguments;
    // +1 here because gcc built-in expects output pointer as param
    call_arguments.reserve (param_vars.size () + 1);
    tree gcc_argument_types = TYPE_ARG_TYPES (TREE_TYPE (gcc_builtin));
    for (Bvariable *param : param_vars)
      {
	tree argument = param->get_tree (locus);
	tree expected_type = TREE_VALUE (gcc_argument_types);
	argument = Backend::convert_expression (expected_type, argument, locus);
	call_arguments.emplace_back (argument);
	gcc_argument_types = TREE_CHAIN (gcc_argument_types);
      }

    // the order of llvm built-ins' tuple return types' fields is not
    // consistent, some are (value, status) and others are (status, value), so
    // we need to reorder it...
    tree tuple_type = TREE_TYPE (DECL_RESULT (fndecl));
    tree first_field = TYPE_FIELDS (tuple_type);
    tree second_field = DECL_CHAIN (first_field);

    tree value_field
      = order == OutputTupleOrder::VALUE_STATUS ? first_field : second_field;
    tree status_field
      = order == OutputTupleOrder::VALUE_STATUS ? second_field : first_field;

    tree value_type = TREE_TYPE (value_field);
    tree status_type = TREE_TYPE (status_field);

    tree temporary_stmt = NULL_TREE;
    Bvariable *value
      = Backend::temporary_variable (fndecl, NULL_TREE, value_type, NULL_TREE,
				     true, locus, &temporary_stmt);
    Bvariable *status
      = Backend::temporary_variable (fndecl, NULL_TREE, status_type, NULL_TREE,
				     false, locus, &temporary_stmt);

    // compile the final gcc built-in param which is the output value pointer,
    // which llvm returns as part of the tuple return type
    tree gcc_pointer_type = TREE_VALUE (gcc_argument_types);
    tree block
      = Backend::block (fndecl, NULL_TREE, {value, status}, locus, locus);
    ctx->push_block (block);

    tree value_decl = value->get_tree (locus);
    tree status_decl = status->get_tree (locus);

    tree value_address = build_fold_addr_expr_loc (locus, value_decl);
    value_address
      = Backend::convert_expression (gcc_pointer_type, value_address, locus);
    call_arguments.emplace_back (value_address);

    // call the gcc built-in with the params that were built and assign the
    // returned val to the status temp var
    tree builtin_call
      = build_call_expr_loc_array (locus, gcc_builtin,
				   static_cast<int> (call_arguments.size ()),
				   call_arguments.data ());
    builtin_call
      = Backend::convert_expression (status_type, builtin_call, locus);
    ctx->add_statement (
      Backend::assignment_statement (status_decl, builtin_call, locus));

    // finally compile and add the return statement
    std::vector<tree> tuple_values;
    if (order == OutputTupleOrder::VALUE_STATUS)
      tuple_values = {value_decl, status_decl};
    else
      tuple_values = {status_decl, value_decl};

    tree tuple_result
      = Backend::constructor_expression (tuple_type, false, tuple_values, -1,
					 locus);
    ctx->add_statement (
      Backend::return_statement (fndecl, tuple_result, locus));

    tree body = ctx->pop_block ();
    DECL_SAVED_TREE (fndecl) = body;

    ctx->push_function (fndecl);
    return fndecl;
  }

  TyTy::BaseType *concrete;
  tree reference;
  location_t ref_locus;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_EXTERN_ITEM
