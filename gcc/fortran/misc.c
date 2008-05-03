/* Miscellaneous stuff that doesn't fit anywhere else.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc.
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
#include "gfortran.h"

/* Get a block of memory.  Many callers assume that the memory we
   return is zeroed.  */

void *
gfc_getmem (size_t n)
{
  void *p;

  if (n == 0)
    return NULL;

  p = xmalloc (n);
  if (p == NULL)
    gfc_fatal_error ("Out of memory-- malloc() failed");
  memset (p, 0, n);
  return p;
}


/* gfortran.h defines free to something that triggers a syntax error,
   but we need free() here.  */

#define temp free
#undef free

void
gfc_free (void *p)
{
  if (p != NULL)
    free (p);
}

#define free temp
#undef temp


/* Get terminal width.  */

int
gfc_terminal_width (void)
{
  return 80;
}


/* Initialize a typespec to unknown.  */

void
gfc_clear_ts (gfc_typespec *ts)
{
  ts->type = BT_UNKNOWN;
  ts->derived = NULL;
  ts->kind = 0;
  ts->cl = NULL;
  ts->interface = NULL;
  /* flag that says if the type is C interoperable */
  ts->is_c_interop = 0;
  /* says what f90 type the C kind interops with */
  ts->f90_type = BT_UNKNOWN;
  /* flag that says whether it's from iso_c_binding or not */
  ts->is_iso_c = 0;
}


/* Open a file for reading.  */

FILE *
gfc_open_file (const char *name)
{
  struct stat statbuf;

  if (!*name)
    return stdin;

  if (stat (name, &statbuf) < 0)
    return NULL;

  if (!S_ISREG (statbuf.st_mode))
    return NULL;

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
    case BT_DERIVED:
      p = "DERIVED";
      break;
    case BT_PROCEDURE:
      p = "PROCEDURE";
      break;
    case BT_VOID:
      p = "VOID";
      break;
    case BT_UNKNOWN:
      p = "UNKNOWN";
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
gfc_typename (gfc_typespec *ts)
{
  static char buffer1[GFC_MAX_SYMBOL_LEN + 7];  /* 7 for "TYPE()" + '\0'.  */
  static char buffer2[GFC_MAX_SYMBOL_LEN + 7];
  static int flag = 0;
  char *buffer;

  buffer = flag ? buffer1 : buffer2;
  flag = !flag;

  switch (ts->type)
    {
    case BT_INTEGER:
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
      sprintf (buffer, "CHARACTER(%d)", ts->kind);
      break;
    case BT_HOLLERITH:
      sprintf (buffer, "HOLLERITH");
      break;
    case BT_DERIVED:
      sprintf (buffer, "TYPE(%s)", ts->derived->name);
      break;
    case BT_PROCEDURE:
      strcpy (buffer, "PROCEDURE");
      break;
    case BT_UNKNOWN:
      strcpy (buffer, "UNKNOWN");
      break;
    default:
      gfc_internal_error ("gfc_typename(): Undefined type");
    }

  return buffer;
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
