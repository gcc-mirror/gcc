/* Stub functions for Objective-C and Objective-C++ routines
   that are called from within the C and C++ front-ends,
   respectively.
   Copyright (C) 1991-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "vec.h"

#include "c-common.h" /* for enum rid.  */
#include "c-objc.h"

tree
objc_is_class_name (tree ARG_UNUSED (arg))
{
  return 0;
}

tree
objc_is_id (tree ARG_UNUSED (arg))
{
  return 0;
}

tree
objc_is_object_ptr (tree ARG_UNUSED (arg))
{
  return 0;
}

bool objc_diagnose_private_ivar (tree ARG_UNUSED (arg))
{
  return false;
}

tree
objc_lookup_ivar (tree other, tree ARG_UNUSED (arg))
{
  /* Just use whatever C/C++ found.  */
  return other;
}

void
objc_check_decl (tree ARG_UNUSED (decl))
{
}

void
objc_check_global_decl (tree ARG_UNUSED (decl))
{
}

tree
objc_common_type (tree ARG_UNUSED (type1), tree ARG_UNUSED (type2))
{
  return 0;
}

bool
objc_compare_types (tree ARG_UNUSED (ltyp), tree ARG_UNUSED (rtyp),
		    int ARG_UNUSED (argno), tree ARG_UNUSED (callee))
{
  return false;
}

bool
objc_have_common_type (tree ARG_UNUSED (ltyp), tree ARG_UNUSED (rtyp),
		       int ARG_UNUSED (argno), tree ARG_UNUSED (callee))
{
  return false;
}

void
objc_volatilize_decl (tree ARG_UNUSED (decl))
{
}

tree
objc_rewrite_function_call (tree function, tree ARG_UNUSED (first_param))
{
  return function;
}

tree
objc_message_selector (void)
{
  return 0;
}

void
objc_declare_alias (tree ARG_UNUSED (alias), tree ARG_UNUSED (orig))
{
}

void
objc_declare_class (tree ARG_UNUSED (identifier))
{
}

void
objc_declare_protocol (tree ARG_UNUSED (name), tree ARG_UNUSED (attributes))
{
}

void
objc_start_protocol (tree ARG_UNUSED (proto),
		     tree ARG_UNUSED (protorefs),
		     tree ARG_UNUSED (attribs))
{
}

void 
objc_set_method_opt (bool ARG_UNUSED (optional))
{
}

void
objc_start_class_interface (tree ARG_UNUSED (name),
			    location_t /*name_loc*/,
			    tree ARG_UNUSED (super),
			    tree ARG_UNUSED (protos),
			    tree ARG_UNUSED (attribs))
{
}

void
objc_start_category_interface (tree ARG_UNUSED (name),
			       tree ARG_UNUSED (categ),
			       tree ARG_UNUSED (protos),
			       tree ARG_UNUSED (attribs))
{
}

void
objc_continue_interface (void)
{
}

void
objc_finish_interface (void)
{
}

void
objc_add_instance_variable (tree ARG_UNUSED (decl))
{
}

void
objc_set_visibility (objc_ivar_visibility_kind ARG_UNUSED (vis))
{
}

void
objc_start_class_implementation (tree ARG_UNUSED (name),
				 tree ARG_UNUSED (super))
{
}

void
objc_start_category_implementation (tree ARG_UNUSED (name),
				    tree ARG_UNUSED (categ))
{
}

void
objc_continue_implementation (void)
{
}

void
objc_clear_super_receiver (void)
{
}

void
objc_finish_implementation (void)
{
}

void
objc_add_method_declaration (bool ARG_UNUSED (is_class_method),
			     tree ARG_UNUSED (signature),
			     tree ARG_UNUSED (attributes))
{
}

bool
objc_start_method_definition (bool ARG_UNUSED (is_class_method),
			      tree ARG_UNUSED (signature),
			      tree ARG_UNUSED (attributes),
			      tree ARG_UNUSED (expr))
{
  return true;
}

void
objc_finish_method_definition (tree ARG_UNUSED (fndecl))
{
}

bool 
objc_method_decl (enum tree_code ARG_UNUSED(opcode))
{
  return false;
}

tree
objc_build_keyword_decl (tree ARG_UNUSED (selector),
			 tree ARG_UNUSED (type),
			 tree ARG_UNUSED (identifier),
			 tree ARG_UNUSED (attributes))
{
  return 0;
}

tree
objc_build_method_signature (bool ARG_UNUSED (is_class_method),
			     tree ARG_UNUSED (rettype),
			     tree ARG_UNUSED (selectors),
			     tree ARG_UNUSED (optparms),
			     bool ARG_UNUSED (ellipsis))
{
  return 0;
}

tree
objc_build_encode_expr (tree ARG_UNUSED (expr))
{
  return 0;
}

tree
objc_build_protocol_expr (tree ARG_UNUSED (expr))
{
  return 0;
}

tree
objc_build_selector_expr (location_t ARG_UNUSED (loc), tree ARG_UNUSED (expr))
{
  return 0;
}

tree
objc_build_message_expr (tree ARG_UNUSED (receiver), tree ARG_UNUSED (args))
{
  return 0;
}

tree
objc_build_string_object (tree ARG_UNUSED (str))
{
  return 0;
}

tree
objc_get_class_reference (tree ARG_UNUSED (name))
{
  return 0;
}

bool
objc_detect_field_duplicates (bool ARG_UNUSED (check_superclasses_only))
{
  return false;
}

tree
objc_get_protocol_qualified_type (tree ARG_UNUSED (name),
				  tree ARG_UNUSED (protos))
{
  return 0;
}

int
objc_static_init_needed_p (void)
{
  return 0;
}

tree
objc_generate_static_init_call (tree ARG_UNUSED (ctors))
{
  return 0;
}

int
objc_is_public (tree ARG_UNUSED (expr), tree ARG_UNUSED (identifier))
{
  return 1;
}

tree
objc_get_class_ivars (tree ARG_UNUSED (name))
{
  return 0;
}

void
objc_add_property_declaration (location_t ARG_UNUSED (location), 
			       tree ARG_UNUSED (decl),
			       vec<property_attribute_info *>&
			       /*prop_attr_list*/)
{
}

bool
objc_is_property_ref (tree ARG_UNUSED (node))
{
  return 0;
}

bool
objc_non_constant_expr_p (tree)
{
  return 0;
}

tree
objc_maybe_build_component_ref (tree ARG_UNUSED (datum), tree ARG_UNUSED (component))
{
  return 0;
}

tree
objc_build_class_component_ref (tree ARG_UNUSED (datum), tree ARG_UNUSED (component))
{
  return 0;
}

tree
objc_maybe_build_modify_expr (tree ARG_UNUSED (lhs), tree ARG_UNUSED (rhs))
{
  return 0;
}

tree
objc_build_incr_expr_for_property_ref (location_t ARG_UNUSED (location),
				       enum tree_code ARG_UNUSED (code),
				       tree ARG_UNUSED (argument),
				       tree ARG_UNUSED (increment))
{
  return 0;
}

void
objc_add_synthesize_declaration (location_t ARG_UNUSED (start_locus), 
				 tree ARG_UNUSED (property_and_ivar_list))
{
}

void
objc_add_dynamic_declaration (location_t ARG_UNUSED (start_locus), 
			      tree ARG_UNUSED (property_list))
{
}

const char *
objc_maybe_printable_name (tree ARG_UNUSED (decl), 
			   int ARG_UNUSED (v))
{
  return NULL;
}

tree
objc_build_throw_stmt (location_t ARG_UNUSED (loc), tree ARG_UNUSED (expr))
{
  return 0;
}

tree
objc_build_synchronized (location_t ARG_UNUSED (start_locus),
			 tree ARG_UNUSED (mutex), tree ARG_UNUSED (body))
{
  return 0;
}

void
objc_begin_try_stmt (location_t ARG_UNUSED (try_locus), tree ARG_UNUSED (body))
{
}

void
objc_begin_catch_clause (tree ARG_UNUSED (decl))
{
}

void
objc_finish_catch_clause (void)
{
}

void
objc_build_finally_clause (location_t ARG_UNUSED (finally_locus),
			   tree ARG_UNUSED (body))
{
}

tree
objc_finish_try_stmt (void)
{
  return 0;
}

tree
objc_generate_write_barrier (tree ARG_UNUSED (lhs),
			     enum tree_code ARG_UNUSED (modifycode),
			     tree ARG_UNUSED (rhs))
{
  return 0;
}

void
objc_finish_foreach_loop (location_t ARG_UNUSED (location), tree ARG_UNUSED (object_expression),
			  tree ARG_UNUSED (collection_expression), tree ARG_UNUSED (for_body),
			  tree ARG_UNUSED (break_label), tree ARG_UNUSED (continue_label))
{
  return;
}

void
objc_write_global_declarations (void)
{
}

bool
objc_string_ref_type_p (tree ARG_UNUSED (strp))
{
   return false;
}

void
objc_check_format_arg (tree ARG_UNUSED (format_arg), 
		       tree ARG_UNUSED (args_list))
{
}

void
objc_finish_function (void)
{
}

void
objc_maybe_warn_exceptions (location_t ARG_UNUSED (loc))
{
}

enum objc_property_attribute_kind objc_prop_attr_kind_for_rid (enum rid)
{
  return OBJC_PROPERTY_ATTR_UNKNOWN;
}
