/* Declarations for objc-actions.c.
   Copyright (C) 1990 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/*** Public Interface (procedures) ***/

/* used by compile_file */

void init_objc (), finish_objc ();

/* used by yyparse */

tree start_class ();
tree continue_class ();
void finish_class ();
void start_method_def ();
void continue_method_def ();
void finish_method_def ();
void add_objc_decls ();

tree is_ivar ();
int  is_public ();
tree add_instance_variable ();
tree add_class_method ();
tree add_instance_method ();
tree get_super_receiver ();
tree get_class_ivars ();
tree get_class_reference ();
tree get_static_reference ();

tree build_message_expr ();
tree build_selector_expr ();
tree build_ivar_reference ();
tree build_keyword_decl ();
tree build_method_decl ();

/* Nonzero enables objc features.  */

extern int doing_objc_thang;

/* the following routines are used to implement statically typed objects */

tree lookup_interface ();
int  objc_comptypes ();
void objc_check_decl ();

/* NeXT extensions */

tree build_encode_expr ();

/* used by rest_of_compilation.  */

void genPrototype ();

/* Objective-C structures */

/* KEYWORD_DECL */
#define KEYWORD_KEY_NAME(DECL) ((DECL)->decl.name)
#define KEYWORD_ARG_NAME(DECL) ((DECL)->decl.arguments)

/* INSTANCE_METHOD_DECL, CLASS_METHOD_DECL */
#define METHOD_SEL_NAME(DECL) ((DECL)->decl.name)
#define METHOD_SEL_ARGS(DECL) ((DECL)->decl.arguments)
#define METHOD_ADD_ARGS(DECL) ((DECL)->decl.result)
#define METHOD_DEFINITION(DECL) ((DECL)->decl.initial)
#define METHOD_ENCODING(DECL) ((DECL)->decl.context)

/* INTERFACE_TYPE, IMPLEMENTATION_TYPE, CATEGORY_TYPE */
#define CLASS_NAME(CLASS) ((CLASS)->type.name)
#define CLASS_SUPER_NAME(CLASS) ((CLASS)->type.binfo)
#define CLASS_IVARS(CLASS) ((CLASS)->type.maxval)
#define CLASS_RAW_IVARS(CLASS) ((CLASS)->type.noncopied_parts)
#define CLASS_NST_METHODS(CLASS) ((CLASS)->type.next_variant)
#define CLASS_CLS_METHODS(CLASS) ((CLASS)->type.main_variant)
#define CLASS_STATIC_TEMPLATE(CLASS) ((CLASS)->type.context)
#define CLASS_CATEGORY_LIST(CLASS) ((CLASS)->type.minval)

/* Define the Objective-C language-specific tree codes.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) SYM,
enum objc_tree_code {
  dummy_tree_code = LAST_AND_UNUSED_TREE_CODE,
#include "objc-tree.def"
  LAST_OBJC_TREE_CODE
};
#undef DEFTREECODE
