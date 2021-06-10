/* d-frontend.cc -- D frontend interface to the gcc back-end.
   Copyright (C) 2013-2020 Free Software Foundation, Inc.

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
#include "dmd/compiler.h"
#include "dmd/declaration.h"
#include "dmd/errors.h"
#include "dmd/expression.h"
#include "dmd/identifier.h"
#include "dmd/module.h"
#include "dmd/mtype.h"
#include "dmd/scope.h"
#include "dmd/statement.h"
#include "dmd/target.h"

#include "tree.h"
#include "options.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "stor-layout.h"

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
  this->errorLimit = flag_max_errors;
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

  return buf.extractString ();
}

bool
Loc::equals (const Loc& loc)
{
  if (this->linnum != loc.linnum || this->charnum != loc.charnum)
    return false;

  if (!FileName::equals (this->filename, loc.filename))
    return false;

  return true;
}


/* Implements the Port interface defined by the frontend.
   A mini library for doing compiler/system specific things.  */

/* Compare the first N bytes of S1 and S2 without regard to the case.  */

int
Port::memicmp (const char *s1, const char *s2, size_t n)
{
  int result = 0;

  for (size_t i = 0; i < n; i++)
    {
      char c1 = s1[i];
      char c2 = s2[i];

      result = c1 - c2;
      if (result)
	{
	  result = TOUPPER (c1) - TOUPPER (c2);
	  if (result)
	    break;
	}
    }

  return result;
}

/* Convert all characters in S to uppercase.  */

char *
Port::strupr (char *s)
{
  char *t = s;

  while (*s)
    {
      *s = TOUPPER (*s);
      s++;
    }

  return t;
}

/* Return true if the real_t value from string BUFFER overflows
   as a result of rounding down to float mode.  */

bool
Port::isFloat32LiteralOutOfRange (const char *buffer)
{
  real_t r;

  real_from_string3 (&r.rv (), buffer, TYPE_MODE (float_type_node));

  return r == Target::RealProperties::infinity;
}

/* Return true if the real_t value from string BUFFER overflows
   as a result of rounding down to double mode.  */

bool
Port::isFloat64LiteralOutOfRange (const char *buffer)
{
  real_t r;

  real_from_string3 (&r.rv (), buffer, TYPE_MODE (double_type_node));

  return r == Target::RealProperties::infinity;
}

/* Fetch a little-endian 16-bit value from BUFFER.  */

unsigned
Port::readwordLE (void *buffer)
{
  unsigned char *p = (unsigned char*) buffer;

  return ((unsigned) p[1] << 8) | (unsigned) p[0];
}

/* Fetch a big-endian 16-bit value from BUFFER.  */

unsigned
Port::readwordBE (void *buffer)
{
  unsigned char *p = (unsigned char*) buffer;

  return ((unsigned) p[0] << 8) | (unsigned) p[1];
}

/* Fetch a little-endian 32-bit value from BUFFER.  */

unsigned
Port::readlongLE (void *buffer)
{
  unsigned char *p = (unsigned char*) buffer;

  return (((unsigned) p[3] << 24)
	  | ((unsigned) p[2] << 16)
	  | ((unsigned) p[1] << 8)
	  | (unsigned) p[0]);
}

/* Fetch a big-endian 32-bit value from BUFFER.  */

unsigned
Port::readlongBE (void *buffer)
{
  unsigned char *p = (unsigned char*) buffer;

  return (((unsigned) p[0] << 24)
	  | ((unsigned) p[1] << 16)
	  | ((unsigned) p[2] << 8)
	  | (unsigned) p[3]);
}

/* Write an SZ-byte sized VALUE to BUFFER, ignoring endian-ness.  */

void
Port::valcpy (void *buffer, uint64_t value, size_t sz)
{
  switch (sz)
    {
    case 1:
      *(uint8_t *) buffer = (uint8_t) value;
      break;

    case 2:
      *(uint16_t *) buffer = (uint16_t) value;
      break;

    case 4:
      *(uint32_t *) buffer = (uint32_t) value;
      break;

    case 8:
      *(uint64_t *) buffer = (uint64_t) value;
      break;

    default:
      gcc_unreachable ();
    }
}


/* Implements the CTFloat interface defined by the frontend.
   Compile-time floating-pointer helper functions.  */

/* Return the absolute value of R.  */

real_t
CTFloat::fabs (real_t r)
{
  real_t x;
  real_arithmetic (&x.rv (), ABS_EXPR, &r.rv (), NULL);
  return x.normalize ();
}

/* Return the value of R * 2 ^^ EXP.  */

real_t
CTFloat::ldexp (real_t r, int exp)
{
  real_t x;
  real_ldexp (&x.rv (), &r.rv (), exp);
  return x.normalize ();
}

/* Return true if longdouble value X is identical to Y.  */

bool
CTFloat::isIdentical (real_t x, real_t y)
{
  real_value rx = x.rv ();
  real_value ry = y.rv ();
  return (REAL_VALUE_ISNAN (rx) && REAL_VALUE_ISNAN (ry))
    || real_identical (&rx, &ry);
}

/* Return true if real_t value R is NaN.  */

bool
CTFloat::isNaN (real_t r)
{
  return REAL_VALUE_ISNAN (r.rv ());
}

/* Same as isNaN, but also check if is signalling.  */

bool
CTFloat::isSNaN (real_t r)
{
  return REAL_VALUE_ISSIGNALING_NAN (r.rv ());
}

/* Return true if real_t value is +Inf.  */

bool
CTFloat::isInfinity (real_t r)
{
  return REAL_VALUE_ISINF (r.rv ());
}

/* Return a real_t value from string BUFFER rounded to long double mode.  */

real_t
CTFloat::parse (const char *buffer, bool *overflow)
{
  real_t r;
  real_from_string3 (&r.rv (), buffer, TYPE_MODE (long_double_type_node));

  /* Front-end checks overflow to see if the value is representable.  */
  if (overflow && r == Target::RealProperties::infinity)
    *overflow = true;

  return r;
}

/* Format the real_t value R to string BUFFER as a decimal or hexadecimal,
   converting the result to uppercase if FMT requests it.  */

int
CTFloat::sprint (char *buffer, char fmt, real_t r)
{
  if (fmt == 'a' || fmt == 'A')
    {
      /* Converting to a hexadecimal string.  */
      real_to_hexadecimal (buffer, &r.rv (), 32, 0, 1);
      int buflen;

      switch (fmt)
	{
	case 'A':
	  buflen = strlen (buffer);
	  for (int i = 0; i < buflen; i++)
	    buffer[i] = TOUPPER (buffer[i]);

	  return buflen;

	case 'a':
	  return strlen (buffer);

	default:
	  gcc_unreachable ();
	}
    }
  else
    {
      /* Note: restricting the precision of significant digits to 18.  */
      real_to_decimal (buffer, &r.rv (), 32, 18, 1);
      return strlen (buffer);
    }
}

/* Return a hash value for real_t value R.  */

size_t
CTFloat::hash (real_t r)
{
  return real_hash (&r.rv ());
}

/* Implements the Compiler interface used by the frontend.  */

/* Generate C main() in response to seeing D main().  This used to be in
   libdruntime, but contained a reference to _Dmain which didn't work when
   druntime was made into a shared library and was linked to a program, such
   as a C++ program, that didn't have a _Dmain.  */

void
Compiler::genCmain (Scope *sc)
{
  static bool initialized = false;

  if (initialized)
    return;

  /* The D code to be generated is provided by __entrypoint.di, try to load it,
     but don't fail if unfound.  */
  unsigned errors = global.startGagging ();
  Module *m = Module::load (Loc (), NULL, Identifier::idPool ("__entrypoint"));

  if (global.endGagging (errors))
    m = NULL;

  if (m != NULL)
    {
      m->importedFrom = m;
      m->importAll (NULL);
      m->semantic (NULL);
      m->semantic2 (NULL);
      m->semantic3 (NULL);
      d_add_entrypoint_module (m, sc->_module);
    }

  initialized = true;
}

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

  if (expr->type->isintegral ())
    cst = build_integer_cst (expr->toInteger (), build_ctype (expr->type));
  else if (expr->type->isfloating ())
    cst = build_float_cst (expr->toReal (), expr->type);
  else if (expr->op == TOKarrayliteral)
    {
      /* Build array as VECTOR_CST, assumes EXPR is constant.  */
      Expressions *elements = ((ArrayLiteralExp *) expr)->elements;
      vec<constructor_elt, va_gc> *elms = NULL;

      vec_safe_reserve (elms, elements->dim);
      for (size_t i = 0; i < elements->dim; i++)
	{
	  Expression *e = (*elements)[i];
	  if (e->type->isintegral ())
	    {
	      tree value = build_integer_cst (e->toInteger (),
					      build_ctype (e->type));
	      CONSTRUCTOR_APPEND_ELT (elms, size_int (i), value);
	    }
	  else if (e->type->isfloating ())
	    {
	      tree value = build_float_cst (e->toReal (), e->type);
	      CONSTRUCTOR_APPEND_ELT (elms, size_int (i), value);
	    }
	  else
	    gcc_unreachable ();
	}

      /* Build vector type.  */
      int nunits = ((TypeSArray *) expr->type)->dim->toUInteger ();
      Type *telem = expr->type->nextOf ();
      tree vectype = build_vector_type (build_ctype (telem), nunits);

      cst = build_vector_from_ctor (vectype, elms);
    }
  else
    gcc_unreachable ();

  /* Encode CST to buffer.  */
  int len = native_encode_expr (cst, buffer, sizeof (buffer));

  if (tb->ty == Tsarray)
    {
      /* Interpret value as a vector of the same size,
	 then return the array literal.  */
      int nunits = ((TypeSArray *) type)->dim->toUInteger ();
      Type *elem = type->nextOf ();
      tree vectype = build_vector_type (build_ctype (elem), nunits);

      cst = native_interpret_expr (vectype, buffer, len);

      Expression *e = d_eval_constant_expression (cst);
      gcc_assert (e != NULL && e->op == TOKvector);

      return ((VectorExp *) e)->e1;
    }
  else
    {
      /* Normal interpret cast.  */
      cst = native_interpret_expr (build_ctype (type), buffer, len);

      Expression *e = d_eval_constant_expression (cst);
      gcc_assert (e != NULL);

      return e;
    }
}

/* Check imported module M for any special processing.
   Modules we look out for are:
    - object: For D runtime type information.
    - gcc.builtins: For all gcc builtins.
    - core.stdc.*: For all gcc library builtins.  */

void
Compiler::loadModule (Module *m)
{
  ModuleDeclaration *md = m->md;

  if (!md || !md->id || !md->packages)
    {
      Identifier *id = (md && md->id) ? md->id : m->ident;
      if (!strcmp (id->toChars (), "object"))
	create_tinfo_types (m);
    }
  else if (md->packages->dim == 1)
    {
      if (!strcmp ((*md->packages)[0]->toChars (), "gcc")
	  && !strcmp (md->id->toChars (), "builtins"))
	d_build_builtins_module (m);
    }
  else if (md->packages->dim == 2)
    {
      if (!strcmp ((*md->packages)[0]->toChars (), "core")
	  && !strcmp ((*md->packages)[1]->toChars (), "stdc"))
	d_add_builtin_module (m);
    }
}

/* Implements back-end specific interfaces used by the frontend.  */

/* Determine return style of function - whether in registers or through a
   hidden pointer to the caller's stack.  */

RET
retStyle (TypeFunction *)
{
  /* Need the backend type to determine this, but this is called from the
     frontend before semantic processing is finished.  An accurate value
     is not currently needed anyway.  */
  return RETstack;
}

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
  if (fd->builtin != BUILTINyes)
    return NULL;

  tree decl = get_symbol_decl (fd);
  gcc_assert (fndecl_built_in_p (decl)
	      || DECL_INTRINSIC_CODE (decl) != INTRINSIC_NONE);

  TypeFunction *tf = (TypeFunction *) fd->type;
  Expression *e = NULL;
  input_location = make_location_t (loc);

  tree result = d_build_call (tf, decl, NULL, arguments);
  result = fold (result);

  /* Builtin should be successfully evaluated.
     Will only return NULL if we can't convert it.  */
  if (TREE_CONSTANT (result) && TREE_CODE (result) != CALL_EXPR)
    e = d_eval_constant_expression (result);

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
