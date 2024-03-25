/* Miscellaneous stuff that doesn't fit anywhere else.
   Copyright (C) 2000-2024 Free Software Foundation, Inc.
   Contributed by Andy Vaught

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
#include "gfortran.h"
#include "spellcheck.h"
#include "tree.h"


/* Initialize a typespec to unknown.  */

void
gfc_clear_ts (gfc_typespec *ts)
{
  ts->type = BT_UNKNOWN;
  ts->u.derived = NULL;
  ts->kind = 0;
  ts->u.cl = NULL;
  ts->interface = NULL;
  /* flag that says if the type is C interoperable */
  ts->is_c_interop = 0;
  /* says what f90 type the C kind interops with */
  ts->f90_type = BT_UNKNOWN;
  /* flag that says whether it's from iso_c_binding or not */
  ts->is_iso_c = 0;
  ts->deferred = false;
}


/* Open a file for reading.  */

FILE *
gfc_open_file (const char *name)
{
  if (!*name)
    return stdin;

  return fopen (name, "r");
}


/* Return a string for each type.  */

const char *
gfc_basic_typename (bt type)
{
  const char *p;

  switch (type)
    {
    case BT_INTEGER:
      p = "INTEGER";
      break;
    case BT_REAL:
      p = "REAL";
      break;
    case BT_COMPLEX:
      p = "COMPLEX";
      break;
    case BT_LOGICAL:
      p = "LOGICAL";
      break;
    case BT_CHARACTER:
      p = "CHARACTER";
      break;
    case BT_HOLLERITH:
      p = "HOLLERITH";
      break;
    case BT_UNION:
      p = "UNION";
      break;
    case BT_DERIVED:
      p = "DERIVED";
      break;
    case BT_CLASS:
      p = "CLASS";
      break;
    case BT_PROCEDURE:
      p = "PROCEDURE";
      break;
    case BT_VOID:
      p = "VOID";
      break;
    case BT_BOZ:
      p = "BOZ";
      break;
    case BT_UNKNOWN:
      p = "UNKNOWN";
      break;
    case BT_ASSUMED:
      p = "TYPE(*)";
      break;
    default:
      gfc_internal_error ("gfc_basic_typename(): Undefined type");
    }

  return p;
}


/* Return a string describing the type and kind of a typespec.  Because
   we return alternating buffers, this subroutine can appear twice in
   the argument list of a single statement.  */

const char *
gfc_typename (gfc_typespec *ts, bool for_hash)
{
  /* Need to add sufficient padding for "TYPE()" + '\0', "UNION()" + '\0',
     or "CLASS()" + '\0'.  */
  static char buffer1[GFC_MAX_SYMBOL_LEN + 8];
  static char buffer2[GFC_MAX_SYMBOL_LEN + 8];
  static int flag = 0;
  char *buffer;
  gfc_charlen_t length = 0;

  buffer = flag ? buffer1 : buffer2;
  flag = !flag;

  switch (ts->type)
    {
    case BT_INTEGER:
      if (ts->f90_type == BT_VOID
	  && ts->u.derived
	  && ts->u.derived->from_intmod == INTMOD_ISO_C_BINDING)
	sprintf (buffer, "TYPE(%s)", ts->u.derived->name);
      else
	sprintf (buffer, "INTEGER(%d)", ts->kind);
      break;
    case BT_REAL:
      sprintf (buffer, "REAL(%d)", ts->kind);
      break;
    case BT_COMPLEX:
      sprintf (buffer, "COMPLEX(%d)", ts->kind);
      break;
    case BT_LOGICAL:
      sprintf (buffer, "LOGICAL(%d)", ts->kind);
      break;
    case BT_CHARACTER:
      if (for_hash)
	{
	  sprintf (buffer, "CHARACTER(%d)", ts->kind);
	  break;
	}

      if (ts->u.cl && ts->u.cl->length)
	length = gfc_mpz_get_hwi (ts->u.cl->length->value.integer);
      if (ts->kind == gfc_default_character_kind)
	sprintf (buffer, "CHARACTER(" HOST_WIDE_INT_PRINT_DEC ")", length);
      else
	sprintf (buffer, "CHARACTER(" HOST_WIDE_INT_PRINT_DEC ",%d)", length,
		 ts->kind);
      break;
    case BT_HOLLERITH:
      sprintf (buffer, "HOLLERITH");
      break;
    case BT_UNION:
      sprintf (buffer, "UNION(%s)", ts->u.derived->name);
      break;
    case BT_DERIVED:
      if (ts->u.derived == NULL)
	{
	  sprintf (buffer, "invalid type");
	  break;
	}
      sprintf (buffer, "TYPE(%s)", ts->u.derived->name);
      break;
    case BT_CLASS:
      if (!ts->u.derived || !ts->u.derived->components
	  || !ts->u.derived->components->ts.u.derived)
	{
	  sprintf (buffer, "invalid class");
	  break;
	}
      if (ts->u.derived->components->ts.u.derived->attr.unlimited_polymorphic)
	sprintf (buffer, "CLASS(*)");
      else
	sprintf (buffer, "CLASS(%s)",
		 ts->u.derived->components->ts.u.derived->name);
      break;
    case BT_ASSUMED:
      sprintf (buffer, "TYPE(*)");
      break;
    case BT_PROCEDURE:
      strcpy (buffer, "PROCEDURE");
      break;
    case BT_BOZ:
      strcpy (buffer, "BOZ");
      break;
    case BT_UNKNOWN:
      strcpy (buffer, "UNKNOWN");
      break;
    default:
      gfc_internal_error ("gfc_typename(): Undefined type");
    }

  return buffer;
}


const char *
gfc_typename (gfc_expr *ex)
{
  /* 34 character buffer: 14 for "CHARACTER(n,4)", n can be upto 20 characters,
     add 19 for the extra width and 1 for '\0' */
  static char buffer1[34];
  static char buffer2[34];
  static bool flag = false;
  char *buffer;
  gfc_charlen_t length;
  buffer = flag ? buffer1 : buffer2;
  flag = !flag;

  if (ex->ts.type == BT_CHARACTER)
    {
      if (ex->expr_type == EXPR_CONSTANT)
	length = ex->value.character.length;
      else if (ex->ts.deferred)
	{
	  if (ex->ts.kind == gfc_default_character_kind)
	    return "CHARACTER(:)";
	  sprintf (buffer, "CHARACTER(:,%d)", ex->ts.kind);
	  return buffer;
	}
      else if (ex->ts.u.cl && ex->ts.u.cl->length == NULL)
	{
	  if (ex->ts.kind == gfc_default_character_kind)
	    return "CHARACTER(*)";
	  sprintf (buffer, "CHARACTER(*,%d)", ex->ts.kind);
	  return buffer;
	}
      else if (ex->ts.u.cl == NULL
	       || ex->ts.u.cl->length->expr_type != EXPR_CONSTANT)
	{
	  if (ex->ts.kind == gfc_default_character_kind)
	    return "CHARACTER";
	  sprintf (buffer, "CHARACTER(KIND=%d)", ex->ts.kind);
	  return buffer;
	}
      else
	length = gfc_mpz_get_hwi (ex->ts.u.cl->length->value.integer);
      if (ex->ts.kind == gfc_default_character_kind)
	sprintf (buffer, "CHARACTER(" HOST_WIDE_INT_PRINT_DEC ")", length);
      else
	sprintf (buffer, "CHARACTER(" HOST_WIDE_INT_PRINT_DEC ",%d)", length,
		 ex->ts.kind);
      return buffer;
    }
  return gfc_typename(&ex->ts);
}

/* The type of a dummy variable can also be CHARACTER(*).  */

const char *
gfc_dummy_typename (gfc_typespec *ts)
{
  static char buffer1[15];  /* 15 for "CHARACTER(*,4)" + '\0'.  */
  static char buffer2[15];
  static bool flag = false;
  char *buffer;

  buffer = flag ? buffer1 : buffer2;
  flag = !flag;

  if (ts->type == BT_CHARACTER)
    {
      bool has_length = false;
      if (ts->u.cl)
	has_length = ts->u.cl->length != NULL;
      if (!has_length)
	{
	  if (ts->kind == gfc_default_character_kind)
	    sprintf(buffer, "CHARACTER(*)");
	  else if (ts->kind >= 0 && ts->kind < 10)
	    sprintf(buffer, "CHARACTER(*,%d)", ts->kind);
	  else
	    sprintf(buffer, "CHARACTER(*,?)");
	  return buffer;
	}
    }
  return gfc_typename(ts);
}


/* Given an mstring array and a code, locate the code in the table,
   returning a pointer to the string.  */

const char *
gfc_code2string (const mstring *m, int code)
{
  while (m->string != NULL)
    {
      if (m->tag == code)
	return m->string;
      m++;
    }

  gfc_internal_error ("gfc_code2string(): Bad code");
  /* Not reached */
}


/* Given an mstring array and a string, returns the value of the tag
   field.  Returns the final tag if no matches to the string are found.  */

int
gfc_string2code (const mstring *m, const char *string)
{
  for (; m->string != NULL; m++)
    if (strcmp (m->string, string) == 0)
      return m->tag;

  return m->tag;
}


/* Convert an intent code to a string.  */
/* TODO: move to gfortran.h as define.  */

const char *
gfc_intent_string (sym_intent i)
{
  return gfc_code2string (intents, i);
}


/***************** Initialization functions ****************/

/* Top level initialization.  */

void
gfc_init_1 (void)
{
  gfc_error_init_1 ();
  gfc_scanner_init_1 ();
  gfc_arith_init_1 ();
  gfc_intrinsic_init_1 ();
}


/* Per program unit initialization.  */

void
gfc_init_2 (void)
{
  gfc_symbol_init_2 ();
  gfc_module_init_2 ();
}


/******************* Destructor functions ******************/

/* Call all of the top level destructors.  */

void
gfc_done_1 (void)
{
  gfc_scanner_done_1 ();
  gfc_intrinsic_done_1 ();
  gfc_arith_done_1 ();
}


/* Per program unit destructors.  */

void
gfc_done_2 (void)
{
  gfc_symbol_done_2 ();
  gfc_module_done_2 ();
}


/* Returns the index into the table of C interoperable kinds where the
   kind with the given name (c_kind_name) was found.  */

int
get_c_kind(const char *c_kind_name, CInteropKind_t kinds_table[])
{
  int index = 0;

  for (index = 0; index < ISOCBINDING_LAST; index++)
    if (strcmp (kinds_table[index].name, c_kind_name) == 0)
      return index;

  return ISOCBINDING_INVALID;
}


/* For a given name TYPO, determine the best candidate from CANDIDATES
   using get_edit_distance.  Frees CANDIDATES before returning.  */

const char *
gfc_closest_fuzzy_match (const char *typo, char **candidates)
{
  /* Determine closest match.  */
  const char *best = NULL;
  char **cand = candidates;
  edit_distance_t best_distance = MAX_EDIT_DISTANCE;
  const size_t tl = strlen (typo);

  while (cand && *cand)
    {
      edit_distance_t dist = get_edit_distance (typo, tl, *cand,
	  strlen (*cand));
      if (dist < best_distance)
	{
	   best_distance = dist;
	   best = *cand;
	}
      cand++;
    }
  /* If more than half of the letters were misspelled, the suggestion is
     likely to be meaningless.  */
  if (best)
    {
      unsigned int cutoff = MAX (tl, strlen (best));

      if (best_distance > cutoff)
	{
	  XDELETEVEC (candidates);
	  return NULL;
	}
      XDELETEVEC (candidates);
    }
  return best;
}

/* Convert between GMP integers (mpz_t) and HOST_WIDE_INT.  */

HOST_WIDE_INT
gfc_mpz_get_hwi (mpz_t op)
{
  /* Using long_long_integer_type_node as that is the integer type
     node that closest matches HOST_WIDE_INT; both are guaranteed to
     be at least 64 bits.  */
  const wide_int w = wi::from_mpz (long_long_integer_type_node, op, true);
  return w.to_shwi ();
}


void
gfc_mpz_set_hwi (mpz_t rop, const HOST_WIDE_INT op)
{
  const wide_int w = wi::shwi (op, HOST_BITS_PER_WIDE_INT);
  wi::to_mpz (w, rop, SIGNED);
}
