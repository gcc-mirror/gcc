/* Declarations for C++ name lookup routines.
   Copyright (C) 2003 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

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

/* Nonzero if this binding is for a local scope, as opposed to a class
   or namespace scope.  */
#define LOCAL_BINDING_P(NODE) ((NODE)->is_local)

/* Nonzero if BINDING_VALUE is from a base class of the class which is
   currently being defined.  */
#define INHERITED_VALUE_BINDING_P(NODE) ((NODE)->value_is_inherited)

/* For a binding between a name and an entity at a non-local scope,
   defines the scope where the binding is declared.  (Either a class
   _TYPE node, or a NAMESPACE_DECL.)  This macro should be used only
   for namespace-level bindings; on the IDENTIFIER_BINDING list
   BINDING_LEVEL is used instead.  */
#define BINDING_SCOPE(NODE) ((NODE)->scope.scope)

/* Nonzero if NODE has BINDING_LEVEL, rather than BINDING_SCOPE.  */
#define BINDING_HAS_LEVEL_P(NODE) ((NODE)->has_level)

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
  union tree_binding_u {
    tree GTY ((tag ("0"))) scope;
    struct cp_binding_level * GTY ((tag ("1"))) level;
  } GTY ((desc ("%0.has_level"))) scope;
  unsigned has_level : 1;
  unsigned value_is_inherited : 1;
  unsigned is_local : 1;
};

extern cxx_binding *cxx_binding_make (tree, tree);
extern void cxx_binding_free (cxx_binding *);



#endif /* GCC_CP_NAME_LOOKUP_H */
