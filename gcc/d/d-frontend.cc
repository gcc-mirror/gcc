/* d-frontend.cc -- D frontend interface to the gcc back-end.
   Copyright (C) 2013-2021 Free Software Foundation, Inc.

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
#include "options.h"
#include "fold-const.h"
#include "diagnostic.h"

#include "d-tree.h"


/* Implements the Global interface defined by the frontend.
   Used for managing the state of the current compilation.  */

Global global;

void
Global::_init (void)
{
  this->mars_ext = "d";
  this->hdr_ext  = "di";
  this->doc_ext  = "html";
  this->ddoc_ext = "ddoc";
  this->json_ext = "json";
  this->obj_ext = "o";

  this->run_noext = true;
  this->version = "v"
#include "verstr.h"
    ;

  this->stdmsg = stderr;
}

/* Start gagging. Return the current number of gagged errors.  */

unsigned
Global::startGagging (void)
{
  this->gag++;
  return this->gaggedErrors;
}

/* End gagging, restoring the old gagged state.  Return true if errors
   occured while gagged.  */

bool
Global::endGagging (unsigned oldGagged)
{
  bool anyErrs = (this->gaggedErrors != oldGagged);
  this->gag--;

  /* Restore the original state of gagged errors; set total errors
     to be original errors + new ungagged errors.  */
  this->errors -= (this->gaggedErrors - oldGagged);
  this->gaggedErrors = oldGagged;

  return anyErrs;
}

/* Increment the error count to record that an error has occured in the
   current context.  An error message may or may not have been printed.  */

void
Global::increaseErrorCount (void)
{
  if (gag)
    this->gaggedErrors++;

  this->errors++;
}


/* Implements the Loc interface defined by the frontend.
   Used for keeping track of current file/line position in code.  */

Loc::Loc (const char *filename, unsigned linnum, unsigned charnum)
{
  this->linnum = linnum;
  this->charnum = charnum;
  this->filename = filename;
}

const char *
Loc::toChars (void) const
{
  OutBuffer buf;

  if (this->filename)
    buf.printf ("%s", this->filename);

  if (this->linnum)
    {
      buf.printf (":%u", this->linnum);
      if (this->charnum)
	buf.printf (":%u", this->charnum);
    }

  return buf.extractChars ();
}

bool
Loc::equals (const Loc &loc)
{
  if (this->linnum != loc.linnum || this->charnum != loc.charnum)
    return false;

  if (!FileName::equals (this->filename, loc.filename))
    return false;

  return true;
}


/* Implements back-end specific interfaces used by the frontend.  */

/* Determine if function FD is a builtin one that we can evaluate in CTFE.  */

BUILTIN
isBuiltin (FuncDeclaration *fd)
{
  if (fd->builtin != BUILTINunknown)
    return fd->builtin;

  maybe_set_intrinsic (fd);

  return fd->builtin;
}

/* Evaluate builtin D function FD whose argument list is ARGUMENTS.
   Return result; NULL if cannot evaluate it.  */

Expression *
eval_builtin (Loc loc, FuncDeclaration *fd, Expressions *arguments)
{
  if (fd->builtin == BUILTINunimp)
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
getTypeInfoType (Loc loc, Type *type, Scope *sc)
{
  gcc_assert (type->ty != Terror);
  check_typeinfo_type (loc, sc);
  create_typeinfo (type, sc ? sc->_module->importedFrom : NULL);
  return type->vtinfo->type;
}

/* Return an inlined copy of a default argument for a function parameter.  */

Expression *
inlineCopy (Expression *e, Scope *)
{
  return e->copy ();
}
