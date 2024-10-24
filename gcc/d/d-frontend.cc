/* d-frontend.cc -- D frontend interface to the gcc back-end.
   Copyright (C) 2013-2024 Free Software Foundation, Inc.

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "dmd/aggregate.h"
#include "dmd/declaration.h"
#include "dmd/expression.h"
#include "dmd/module.h"
#include "dmd/mtype.h"
#include "dmd/scope.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"

#include "d-tree.h"
#include "d-frontend.h"

/* Implements back-end specific interfaces used by the frontend.  */

/* Determine if function FD is a builtin one that we can evaluate in CTFE.  */

BUILTIN
isBuiltin (FuncDeclaration *fd)
{
  if (fd->builtin != BUILTIN::unknown)
    return fd->builtin;

  maybe_set_intrinsic (fd);

  return fd->builtin;
}

/* Evaluate builtin D function FD whose argument list is ARGUMENTS.
   Return result; NULL if cannot evaluate it.  */

Expression *
eval_builtin (const Loc &loc, FuncDeclaration *fd, Expressions *arguments)
{
  if (fd->builtin == BUILTIN::unimp)
    return NULL;

  tree decl = get_symbol_decl (fd);
  gcc_assert (fndecl_built_in_p (decl)
	      || DECL_INTRINSIC_CODE (decl) != INTRINSIC_NONE);

  TypeFunction *tf = fd->type->toTypeFunction ();
  Expression *e = NULL;
  input_location = make_location_t (loc);

  tree result = d_build_call (tf, decl, NULL, arguments);
  result = fold (result);

  /* Builtin should be successfully evaluated.
     Will only return NULL if we can't convert it.  */
  if (TREE_CONSTANT (result) && TREE_CODE (result) != CALL_EXPR)
    e = d_eval_constant_expression (loc, result);

  return e;
}

/* Build and return typeinfo type for TYPE.  */

Type *
getTypeInfoType (const Loc &loc, Type *type, Scope *sc, bool genObjCode)
{
  gcc_assert (type->ty != TY::Terror);
  check_typeinfo_type (loc, sc);
  create_typeinfo (type, sc ? sc->_module->importedFrom : NULL, genObjCode);
  return type->vtinfo->type;
}

void
toObjFile (Dsymbol *ds, bool)
{
  build_decl_tree (ds);
}
