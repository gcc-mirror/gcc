/* Support routines shared by all runtimes.
   Copyright (C) 2011-2023 Free Software Foundation, Inc.
   Contributed by Iain Sandoe

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef _OBJC_RUNTIME_SHARED_SUPPORT_H_
#define _OBJC_RUNTIME_SHARED_SUPPORT_H_

/* Left in obj-act.c for now... */

extern tree objc_start_struct (tree);
extern tree objc_finish_struct (tree, tree);
extern tree add_field_decl (tree, const char *, tree **);
extern tree build_sized_array_type (tree, int);

extern tree lookup_interface (tree);
extern tree objc_build_constructor (tree, vec<constructor_elt, va_gc> *);

extern tree start_var_decl (tree, const char *);
extern void finish_var_decl (tree, tree);

extern tree my_build_string (int, const char *);
extern tree my_build_string_pointer (int, const char *);
extern tree add_objc_string (tree ident, string_section);
extern tree get_objc_string_decl (tree, string_section);

extern void add_class_reference (tree);

#ifdef OBJCPLUS
extern void objc_start_function (tree, tree, tree, tree);
extern tree objc_get_parm_info (int, tree);
#else
extern void objc_start_function (tree, tree, tree, struct c_arg_info *);
extern struct c_arg_info *objc_get_parm_info (int, tree);
#endif
extern void objc_push_parm (tree);

extern tree build_function_type_for_method (tree, tree, int, bool);

extern char *objc_build_property_setter_name (tree);

/* Stuff that should be migrated to shared support (or some v1-only file).  */
extern void build_super_template (void);

extern tree objc_build_component_ref (tree, tree);

extern tree build_descriptor_table_initializer (tree, tree);
extern tree build_method_prototype_list_template (tree, int);
extern tree build_protocol_initializer (tree, tree, tree, tree, tree);

/* Moved or new routines in objc-runtime-shared-support.cc  */

extern tree build_selector (tree);
extern tree build_method_template (void);
extern tree build_method_prototype_template (void);
extern tree build_method_list_template (tree, int);

extern tree build_dispatch_table_initializer (tree, tree);
extern void diagnose_missing_method (tree, location_t);
extern void build_next_selector_translation_table (void);
extern void generate_protocol_references (tree);
extern void build_module_descriptor (long, tree);
extern tree build_ivar_list_initializer (tree, tree);
extern tree build_ivar_list_template (tree, int);
extern tree build_ivar_template (void);

extern void generate_strings (void);

extern void dump_interface (FILE *, tree);
extern FILE *gen_declaration_file;

#endif /* _OBJC_RUNTIME_SHARED_SUPPORT_H_ */
