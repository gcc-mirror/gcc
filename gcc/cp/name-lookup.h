/* Declarations for C++ name lookup routines.
   Copyright (C) 2003 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef GCC_CP_NAME_LOOKUP_H
#define GCC_CP_NAME_LOOKUP_H

#include "c-common.h"

/* Datatype used to temporarily save C++ bindings (for implicit
   instantiations purposes and like).  Implemented in decl.c.  */
typedef struct cxx_saved_binding cxx_saved_binding;

/* Datatype that represents binding established by a declaration between
   a name and a C++ entity.  */
typedef struct cxx_binding cxx_binding;

/* The datatype used to implement C++ scope.  */
typedef struct cp_binding_level cxx_scope;

/* Nonzero if this binding is for a local scope, as opposed to a class
   or namespace scope.  */
#define LOCAL_BINDING_P(NODE) ((NODE)->is_local)

/* Nonzero if BINDING_VALUE is from a base class of the class which is
   currently being defined.  */
#define INHERITED_VALUE_BINDING_P(NODE) ((NODE)->value_is_inherited)

/* For a binding between a name and an entity at a non-local scope,
   defines the scope where the binding is declared.  (Either a class
   _TYPE node, or a NAMESPACE_DECL.).  */
#define BINDING_SCOPE(NODE) ((NODE)->scope)

/* This is the declaration bound to the name. Possible values:
   variable, overloaded function, namespace, template, enumerator.  */
#define BINDING_VALUE(NODE) ((NODE)->value)

/* If name is bound to a type, this is the type (struct, union, enum).  */
#define BINDING_TYPE(NODE)   ((NODE)->type)

/* Zero out a cxx_binding pointed to by B.  */
#define cxx_binding_clear(B) memset ((B), 0, sizeof (cxx_binding))

struct cxx_binding GTY(())
{
  /* Link to chain together various bindings for this name.  */
  cxx_binding *previous;
  /* The non-type entity this name is bound to.  */
  tree value;
  /* The type entity this name is bound to.  */
  tree type;
  /* The scope at which this binding was made.  */
  cxx_scope *scope;
  unsigned value_is_inherited : 1;
  unsigned is_local : 1;
};

extern cxx_binding *cxx_binding_make (tree, tree);
extern void cxx_binding_free (cxx_binding *);


extern cxx_binding *cxx_scope_find_binding_for_name (cxx_scope *, tree);
extern cxx_binding *binding_for_name (cxx_scope *, tree);

extern tree namespace_binding (tree, tree);
extern void set_namespace_binding (tree, tree, tree);

#endif /* GCC_CP_NAME_LOOKUP_H */
