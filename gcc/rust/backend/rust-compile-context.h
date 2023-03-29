// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#ifndef RUST_COMPILE_CONTEXT
#define RUST_COMPILE_CONTEXT

#include "rust-system.h"
#include "rust-hir-map.h"
#include "rust-name-resolver.h"
#include "rust-hir-type-check.h"
#include "rust-backend.h"
#include "rust-hir-full.h"
#include "rust-mangle.h"
#include "rust-tree.h"

namespace Rust {
namespace Compile {

struct fncontext
{
  tree fndecl;
  ::Bvariable *ret_addr;
};

class Context
{
public:
  Context (::Backend *backend);

  void setup_builtins ();

  bool lookup_compiled_types (tree t, tree *type)
  {
    hashval_t h = type_hasher (t);
    auto it = compiled_type_map.find (h);
    if (it == compiled_type_map.end ())
      return false;

    *type = it->second;
    return true;
  }

  tree insert_compiled_type (tree type)
  {
    hashval_t h = type_hasher (type);
    auto it = compiled_type_map.find (h);
    if (it != compiled_type_map.end ())
      return it->second;

    compiled_type_map.insert ({h, type});
    push_type (type);
    return type;
  }

  tree insert_main_variant (tree type)
  {
    hashval_t h = type_hasher (type);
    auto it = main_variants.find (h);
    if (it != main_variants.end ())
      return it->second;

    main_variants.insert ({h, type});
    return type;
  }

  ::Backend *get_backend () { return backend; }
  Resolver::Resolver *get_resolver () { return resolver; }
  Resolver::TypeCheckContext *get_tyctx () { return tyctx; }
  Analysis::Mappings *get_mappings () { return mappings; }

  void push_block (tree scope)
  {
    scope_stack.push_back (scope);
    statements.push_back ({});
  }

  tree pop_block ()
  {
    auto block = scope_stack.back ();
    scope_stack.pop_back ();

    auto stmts = statements.back ();
    statements.pop_back ();

    backend->block_add_statements (block, stmts);

    return block;
  }

  tree peek_enclosing_scope ()
  {
    if (scope_stack.size () == 0)
      return nullptr;

    return scope_stack.back ();
  }

  void add_statement_to_enclosing_scope (tree stmt)
  {
    statements.at (statements.size () - 2).push_back (stmt);
  }

  void add_statement (tree stmt) { statements.back ().push_back (stmt); }

  void insert_var_decl (HirId id, ::Bvariable *decl)
  {
    compiled_var_decls[id] = decl;
  }

  bool lookup_var_decl (HirId id, ::Bvariable **decl)
  {
    auto it = compiled_var_decls.find (id);
    if (it == compiled_var_decls.end ())
      return false;

    *decl = it->second;
    return true;
  }

  void insert_function_decl (const TyTy::FnType *ref, tree fn)
  {
    auto id = ref->get_ty_ref ();
    auto dId = ref->get_id ();

    rust_assert (compiled_fn_map.find (id) == compiled_fn_map.end ());
    compiled_fn_map[id] = fn;

    auto it = mono_fns.find (dId);
    if (it == mono_fns.end ())
      mono_fns[dId] = {};

    mono_fns[dId].push_back ({ref, fn});
  }

  void insert_closure_decl (const TyTy::ClosureType *ref, tree fn)
  {
    auto dId = ref->get_def_id ();
    auto it = mono_closure_fns.find (dId);
    if (it == mono_closure_fns.end ())
      mono_closure_fns[dId] = {};

    mono_closure_fns[dId].push_back ({ref, fn});
  }

  tree lookup_closure_decl (const TyTy::ClosureType *ref)
  {
    auto dId = ref->get_def_id ();
    auto it = mono_closure_fns.find (dId);
    if (it == mono_closure_fns.end ())
      return error_mark_node;

    for (auto &i : it->second)
      {
	const TyTy::ClosureType *t = i.first;
	tree fn = i.second;

	if (ref->is_equal (*t))
	  return fn;
      }

    return error_mark_node;
  }

  bool lookup_function_decl (HirId id, tree *fn, DefId dId = UNKNOWN_DEFID,
			     const TyTy::BaseType *ref = nullptr,
			     const std::string &asm_name = std::string ())
  {
    // for for any monomorphized fns
    if (ref != nullptr)
      {
	rust_assert (dId != UNKNOWN_DEFID);

	auto it = mono_fns.find (dId);
	if (it == mono_fns.end ())
	  return false;

	for (auto &e : mono_fns[dId])
	  {
	    const TyTy::BaseType *r = e.first;
	    tree f = e.second;

	    if (ref->is_equal (*r))
	      {
		*fn = f;
		return true;
	      }

	    if (DECL_ASSEMBLER_NAME_SET_P (f) && !asm_name.empty ())
	      {
		tree raw = DECL_ASSEMBLER_NAME_RAW (f);
		const char *rptr = IDENTIFIER_POINTER (raw);

		bool lengths_match_p
		  = IDENTIFIER_LENGTH (raw) == asm_name.size ();
		if (lengths_match_p
		    && strncmp (rptr, asm_name.c_str (),
				IDENTIFIER_LENGTH (raw))
			 == 0)
		  {
		    *fn = f;
		    return true;
		  }
	      }
	  }
	return false;
      }

    auto it = compiled_fn_map.find (id);
    if (it == compiled_fn_map.end ())
      return false;

    *fn = it->second;
    return true;
  }

  void insert_const_decl (HirId id, tree expr) { compiled_consts[id] = expr; }

  bool lookup_const_decl (HirId id, tree *expr)
  {
    auto it = compiled_consts.find (id);
    if (it == compiled_consts.end ())
      return false;

    *expr = it->second;
    return true;
  }

  void insert_label_decl (HirId id, tree label) { compiled_labels[id] = label; }

  bool lookup_label_decl (HirId id, tree *label)
  {
    auto it = compiled_labels.find (id);
    if (it == compiled_labels.end ())
      return false;

    *label = it->second;
    return true;
  }

  void insert_pattern_binding (HirId id, tree binding)
  {
    implicit_pattern_bindings[id] = binding;
  }

  bool lookup_pattern_binding (HirId id, tree *binding)
  {
    auto it = implicit_pattern_bindings.find (id);
    if (it == implicit_pattern_bindings.end ())
      return false;

    *binding = it->second;
    return true;
  }

  void push_fn (tree fn, ::Bvariable *ret_addr)
  {
    fn_stack.push_back (fncontext{fn, ret_addr});
  }
  void pop_fn () { fn_stack.pop_back (); }

  bool in_fn () { return fn_stack.size () != 0; }

  // Note: it is undefined behavior to call peek_fn () if fn_stack is empty.
  fncontext peek_fn ()
  {
    rust_assert (!fn_stack.empty ());
    return fn_stack.back ();
  }

  void push_type (tree t) { type_decls.push_back (t); }
  void push_var (::Bvariable *v) { var_decls.push_back (v); }
  void push_const (tree c) { const_decls.push_back (c); }
  void push_function (tree f) { func_decls.push_back (f); }

  void write_to_backend ()
  {
    backend->write_global_definitions (type_decls, const_decls, func_decls,
				       var_decls);
  }

  bool function_completed (tree fn)
  {
    for (auto it = func_decls.begin (); it != func_decls.end (); it++)
      {
	tree i = (*it);
	if (i == fn)
	  {
	    return true;
	  }
      }
    return false;
  }

  void push_loop_context (Bvariable *var) { loop_value_stack.push_back (var); }

  Bvariable *peek_loop_context () { return loop_value_stack.back (); }

  Bvariable *pop_loop_context ()
  {
    auto back = loop_value_stack.back ();
    loop_value_stack.pop_back ();
    return back;
  }

  void push_loop_begin_label (tree label)
  {
    loop_begin_labels.push_back (label);
  }

  tree peek_loop_begin_label () { return loop_begin_labels.back (); }

  tree pop_loop_begin_label ()
  {
    tree pop = loop_begin_labels.back ();
    loop_begin_labels.pop_back ();
    return pop;
  }

  void push_const_context (void) { const_context++; }
  void pop_const_context (void)
  {
    if (const_context > 0)
      const_context--;
  }
  bool const_context_p (void) { return (const_context > 0); }

  std::string mangle_item (const TyTy::BaseType *ty,
			   const Resolver::CanonicalPath &path) const
  {
    return mangler.mangle_item (ty, path);
  }

  void push_closure_context (HirId id);
  void pop_closure_context ();
  void insert_closure_binding (HirId id, tree expr);
  bool lookup_closure_binding (HirId id, tree *expr);

  std::vector<tree> &get_type_decls () { return type_decls; }
  std::vector<::Bvariable *> &get_var_decls () { return var_decls; }
  std::vector<tree> &get_const_decls () { return const_decls; }
  std::vector<tree> &get_func_decls () { return func_decls; }

  static hashval_t type_hasher (tree type);

private:
  ::Backend *backend;
  Resolver::Resolver *resolver;
  Resolver::TypeCheckContext *tyctx;
  Analysis::Mappings *mappings;
  Mangler mangler;

  // state
  std::vector<fncontext> fn_stack;
  std::map<HirId, ::Bvariable *> compiled_var_decls;
  std::map<hashval_t, tree> compiled_type_map;
  std::map<HirId, tree> compiled_fn_map;
  std::map<HirId, tree> compiled_consts;
  std::map<HirId, tree> compiled_labels;
  std::vector<::std::vector<tree>> statements;
  std::vector<tree> scope_stack;
  std::vector<::Bvariable *> loop_value_stack;
  std::vector<tree> loop_begin_labels;
  std::map<DefId, std::vector<std::pair<const TyTy::BaseType *, tree>>>
    mono_fns;
  std::map<DefId, std::vector<std::pair<const TyTy::ClosureType *, tree>>>
    mono_closure_fns;
  std::map<HirId, tree> implicit_pattern_bindings;
  std::map<hashval_t, tree> main_variants;

  // closure bindings
  std::vector<HirId> closure_scope_bindings;
  std::map<HirId, std::map<HirId, tree>> closure_bindings;

  // To GCC middle-end
  std::vector<tree> type_decls;
  std::vector<::Bvariable *> var_decls;
  std::vector<tree> const_decls;
  std::vector<tree> func_decls;

  // Nonzero iff we are currently compiling something inside a constant context.
  unsigned int const_context = 0;
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_CONTEXT
