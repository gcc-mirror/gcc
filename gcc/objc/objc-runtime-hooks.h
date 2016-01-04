/* Hooks to abstract the runtime meta-data generation for Objective C.
   Copyright (C) 2011-2016 Free Software Foundation, Inc.
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

#ifndef _OBJC_RUNTIME_HOOKS_H_
#define _OBJC_RUNTIME_HOOKS_H_

/* A set of hooks for the front end to obtain runtime-specific actions.  */

/* Objective-C supports several runtime library variants:

   "GNU" runtime selected by -fgnu-runtime (currently at ABI version 8).
   "NeXT" runtime (selected by -fnext-runtime) and installed on OSX/Darwin
   systems at API version 1 (for m32 code) and version 2 (for m64 code).

   The runtimes require different data types/layouts, method call mechanisms
   and so on, and the purpose of this interface is to abstract such
   differences from the parser's perspective.  */

/* TODO: Do we want the initial underscore ? */
struct objc_runtime_hooks
{
  /* TODO: Expand comments in this file.  */

  /* Initialize for this runtime.  */
  void (*initialize) (void);
  const char *default_constant_string_class_name;

  /* FIXME: Having to check this name should not be necessary.  */
  const char *tag_getclass;
  /* id for superclass class field - named differently in the existing
     runtimes.  */
  tree (*super_superclassfield_ident) (void);

  /* Obtain a class decl for the identifier.  */
  tree (*class_decl) (tree);
  /* Obtain a metaclass decl for the identifier.  */
  tree (*metaclass_decl) (tree);
  /* Obtain a category decl for the identifier.  */
  tree (*category_decl) (tree);
  /* Obtain a protocol decl for the identifier.  */
  tree (*protocol_decl) (tree);
  /* Obtain a string decl, to be placed in the nominated string-section.  */
  tree (*string_decl) (tree, const char *, string_section);

  /* Obtain a class reference, generating the fwd def. if necessary.  */
  tree (*get_class_reference) (tree);
  /* build/get selector reference.  */
  tree (*build_selector_reference) (location_t, tree, tree);
  /* Get a protocol reference, generating the forward def. if necessary.  */
  tree (*get_protocol_reference) (location_t, tree);
  /* Get an ivar ref. re the base.  */
  tree (*build_ivar_reference) (location_t, tree, tree);
  /* Get a reference to {meta}class' super.  */
  tree (*get_class_super_ref) (location_t, struct imp_entry *, bool);
  /* Get a reference to Category {meta}class' super.  */
  tree (*get_category_super_ref) (location_t, struct imp_entry *, bool);

  /* Receiver is class Object, check runtime-specific.  */
  tree (*receiver_is_class_object) (tree);
  /* Get the start of a method argument type list (receiver, _cmd).  */
  void (*get_arg_type_list_base) (vec<tree, va_gc> **, tree, int, int);
  /* Build method call.  */
  tree (*build_objc_method_call) (location_t, tree, tree, tree, tree, tree, int);

  /* Check for or otherwise handle a request to check that the constant
     string class reference is set-up & OK.  */
  bool (*setup_const_string_class_decl) (void);
  /* Return the tree reprenting a const string constructor for the arg.
     Most of the data are in global trees.  */
  tree (*build_const_string_constructor) (location_t, tree, int);

  /* Exceptions.  */
  tree (*build_throw_stmt) (location_t, tree, bool);
  tree (*build_exc_ptr) (struct objc_try_context **);
  tree (*begin_catch) (struct objc_try_context **, tree, tree, tree, bool);
  void (*finish_catch) (struct objc_try_context **, tree);
  tree (*finish_try_stmt) (struct objc_try_context **);

  /* Emit all the metadata required by the runtime - based on the tables built
     during parsing.  */
  void (*generate_metadata) (void);

};

/* For shared support that needs to access these.  */
extern objc_runtime_hooks runtime;

/* One per runtime at present.
   TODO: Make into some kind of configury-generated table.  */
extern bool objc_gnu_runtime_abi_01_init (objc_runtime_hooks *);
extern bool objc_next_runtime_abi_01_init (objc_runtime_hooks *);
extern bool objc_next_runtime_abi_02_init (objc_runtime_hooks *);

#endif /* _OBJC_RUNTIME_HOOKS_H_ */
