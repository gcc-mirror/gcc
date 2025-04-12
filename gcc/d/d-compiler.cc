/* d-compiler.cc -- D frontend interface to the gcc back-end.
   Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#include "dmd/compiler.h"
#include "dmd/errors.h"
#include "dmd/expression.h"
#include "dmd/identifier.h"
#include "dmd/module.h"
#include "dmd/mtype.h"

#include "tree.h"
#include "fold-const.h"

#include "d-tree.h"


/* Implements the Compiler interface used by the frontend.  */

/* Perform a reinterpret cast of EXPR to type TYPE for use in CTFE.
   The front end should have already ensured that EXPR is a constant,
   so we just lower the value to GCC and return the converted CST.  */

Expression *
Compiler::paintAsType (UnionExp *, Expression *expr, Type *type)
{
  /* We support up to 512-bit values.  */
  unsigned char buffer[64];
  tree cst;

  Type *tb = type->toBasetype ();

  if (expr->type->isIntegral ())
    cst = build_integer_cst (expr->toInteger (), build_ctype (expr->type));
  else if (expr->type->isFloating ())
    cst = build_float_cst (expr->toReal (), expr->type);
  else if (expr->op == EXP::arrayLiteral)
    {
      /* Build array as VECTOR_CST, assumes EXPR is constant.  */
      Expressions *elements = expr->isArrayLiteralExp ()->elements;
      vec <constructor_elt, va_gc> *elms = NULL;

      vec_safe_reserve (elms, elements->length);
      for (size_t i = 0; i < elements->length; i++)
	{
	  Expression *e = (*elements)[i];
	  if (e->type->isIntegral ())
	    {
	      tree value = build_integer_cst (e->toInteger (),
					      build_ctype (e->type));
	      CONSTRUCTOR_APPEND_ELT (elms, size_int (i), value);
	    }
	  else if (e->type->isFloating ())
	    {
	      tree value = build_float_cst (e->toReal (), e->type);
	      CONSTRUCTOR_APPEND_ELT (elms, size_int (i), value);
	    }
	  else
	    gcc_unreachable ();
	}

      /* Build vector type.  */
      int nunits = expr->type->isTypeSArray ()->dim->toUInteger ();
      Type *telem = expr->type->nextOf ();
      tree vectype = build_vector_type (build_ctype (telem), nunits);

      cst = build_vector_from_ctor (vectype, elms);
    }
  else
    gcc_unreachable ();

  /* Encode CST to buffer.  */
  int len = native_encode_expr (cst, buffer, sizeof (buffer));

  if (tb->ty == TY::Tsarray)
    {
      /* Interpret value as a vector of the same size,
	 then return the array literal.  */
      int nunits = type->isTypeSArray ()->dim->toUInteger ();
      Type *elem = type->nextOf ();
      tree vectype = build_vector_type (build_ctype (elem), nunits);

      cst = native_interpret_expr (vectype, buffer, len);

      Expression *e = d_eval_constant_expression (expr->loc, cst);
      gcc_assert (e != NULL && e->op == EXP::vector);

      return e->isVectorExp ()->e1;
    }
  else
    {
      /* Normal interpret cast.  */
      cst = native_interpret_expr (build_ctype (type), buffer, len);

      Expression *e = d_eval_constant_expression (expr->loc, cst);
      gcc_assert (e != NULL);

      return e;
    }
}

/* Check imported module M for any special processing.
   Modules we look out for are:
    - object: For D runtime type information.
    - gcc.builtins: For all gcc builtins.
    - all other modules for extern(C) gcc library builtins.  */

void
Compiler::onParseModule (Module *m)
{
  ModuleDeclaration *md = m->md;

  if (md && md->id)
    {
      if (md->packages.length == 0)
	{
	  if (!strcmp (md->id->toChars (), "object"))
	    {
	      create_tinfo_types (m);
	      return;
	    }
	}
      else if (md->packages.length == 1)
	{
	  if (!strcmp (md->packages.ptr[0]->toChars (), "gcc")
	      && !strcmp (md->id->toChars (), "builtins"))
	    {
	      d_build_builtins_module (m);
	      return;
	    }
	}
    }
  else if (m->ident)
    {
      if (!strcmp (m->ident->toChars (), "object"))
	{
	  create_tinfo_types (m);
	  return;
	}
    }

  if (!flag_no_builtin)
    d_add_builtin_module (m);
}

/* A callback function that is called once an imported module is parsed.
   If the callback returns true, then it tells the front-end that the
   driver intends on compiling the import.  */

bool
Compiler::onImport (Module *m)
{
  if (!includeImports)
    return false;

  if (m->filetype != FileType::d && m->filetype != FileType::c)
    return false;

  /* All imports modules are included except those in the runtime library.  */
  ModuleDeclaration *md = m->md;
  if (md && md->id)
    {
      if (md->packages.length >= 1)
	{
	  if (!strcmp (md->packages.ptr[0]->toChars (), "core")
	      || !strcmp (md->packages.ptr[0]->toChars (), "std")
	      || !strcmp (md->packages.ptr[0]->toChars (), "gcc")
	      || !strcmp (md->packages.ptr[0]->toChars (), "etc"))
	    return false;
	}
      else if (!strcmp (md->id->toChars (), "object"))
	return false;
    }
  else if (m->ident)
    {
      if (!strcmp (m->ident->toChars (), "object"))
	return false;
    }

  /* This import will be compiled.  */
  if (global.params.v.verbose)
    message ("compileimport (%s)", m->srcfile.toChars ());

  compiledImports.push (m);
  return true;
}
