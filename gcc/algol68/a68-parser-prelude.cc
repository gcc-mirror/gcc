/* Standard prelude definitions.
   Copyright (C) 2001-2023 J. Marcel van der Veer.
   Copyright (C) 2025 Jose E. Marchesi.

   Original implementation by J. Marcel van der Veer.
   Adapted for GCC by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "a68.h"

#define A68_STD true
#define A68_EXT false

/* ALGOL68C type procs. */

#define A68C_DEFIO(name, mode)					\
  do								\
    {								\
      m = a68_proc (MODE (mode), M_REF_FILE, NO_MOID);		\
      a68_idf (A68_EXT, "get" #name, m);			\
      m = a68_proc (M_VOID, M_REF_FILE, MODE (mode), NO_MOID);	\
      a68_idf (A68_EXT, "put" #name, m);			\
      m = a68_proc (MODE (mode), NO_MOID);			\
      a68_idf (A68_EXT, "read" #name, m);			\
      m = a68_proc (M_VOID, MODE (mode), NO_MOID);		\
      a68_idf (A68_EXT, "print" #name, m);			\
    }								\
  while (0)

/* Enter tag in standenv symbol table.  */

static void
add_a68_standenv (bool portable, int a, NODE_T* n, char *c, MOID_T *m,
		  int p, LOWERER_T l = NO_LOWERER)
{
#define INSERT_TAG(l, n) \
  do {			 \
    NEXT (n) = *(l);	 \
    *(l) = (n);		 \
  } while (0)

  TAG_T *new_one = a68_new_tag ();

  PROCEDURE_LEVEL (INFO (n)) = 0;
  USE (new_one) = false;
  HEAP (new_one) = HEAP_SYMBOL;
  TAG_TABLE (new_one) = A68_STANDENV;
  NODE (new_one) = n;
  if (c != NO_TEXT)
    VALUE (new_one) = TEXT (a68_add_token (&A68 (top_token), c));
  else
    VALUE (new_one) = NO_TEXT;
  PRIO (new_one) = p;
  TAX_TREE_DECL (new_one) = NULL;
  LOWERER (new_one) = l;
  UNIT (new_one) = NULL;
  PORTABLE (new_one) = portable;
  MOID (new_one) = m;
  NEXT (new_one) = NO_TAG;
  if (a == IDENTIFIER)
    INSERT_TAG (&IDENTIFIERS (A68_STANDENV), new_one);
  else if (a == OP_SYMBOL)
    INSERT_TAG (&OPERATORS (A68_STANDENV), new_one);
  else if (a == PRIO_SYMBOL)
    INSERT_TAG (&PRIO (A68_STANDENV), new_one);
  else if (a == INDICANT)
    INSERT_TAG (&INDICANTS (A68_STANDENV), new_one);
  else if (a == LABEL)
    INSERT_TAG (&LABELS (A68_STANDENV), new_one);
#undef INSERT_TAG
}

/* Compose PROC moid from arguments - first result, than arguments.  */

static MOID_T *
a68_proc (MOID_T *m, ...)
{
  PACK_T *p = NO_PACK, *q = NO_PACK;
  MOID_T *y;

  va_list attribute;
  va_start (attribute, m);
  while ((y = va_arg (attribute, MOID_T *)) != NO_MOID)
    {
      PACK_T *new_one = a68_new_pack ();

      MOID (new_one) = y;
      TEXT (new_one) = NO_TEXT;
      NEXT (new_one) = NO_PACK;
      if (q != NO_PACK)
	NEXT (q) = new_one;
      else
	p = new_one;
      q = new_one;
    }

  va_end (attribute);
  return a68_add_mode (&TOP_MOID (&A68_JOB), PROC_SYMBOL,
		   a68_count_pack_members (p), NO_NODE, m, p);
}

/* Enter an identifier in standenv.  */

static void
a68_idf (bool portable, const char *n, MOID_T *m, LOWERER_T l = NO_LOWERER)
{
  add_a68_standenv (portable, IDENTIFIER,
		    a68_some_node (TEXT (a68_add_token (&A68 (top_token), n))),
		    NO_TEXT, m, 0, l);
}

/* Enter a moid in standenv.  */

static void
a68_mode (int p, const char *t, MOID_T **m)
{
  (*m) = a68_add_mode (&TOP_MOID (&A68_JOB),
		   STANDARD, p,
		   a68_some_node (TEXT (a68_find_keyword (A68 (top_keyword), t))),
		   NO_MOID, NO_PACK);
}

/* Enter a priority in standenv.  */

static void
a68_prio (const char *p, int b)
{
  add_a68_standenv (true, PRIO_SYMBOL,
		    a68_some_node (TEXT (a68_add_token (&A68 (top_token), p))),
		    NO_TEXT, NO_MOID, b, NO_LOWERER);
}

/* Enter operator in standenv.  */

static void
a68_op (bool portable, const char *n, MOID_T *m, LOWERER_T l = NO_LOWERER)
{
  add_a68_standenv (portable, OP_SYMBOL,
		    a68_some_node (TEXT (a68_add_token (&A68 (top_token), n))),
		    NO_TEXT, m, 0, l);
}

/* Enter standard modes in standenv.  */

static void
stand_moids (void)
{
  /* Primitive A68 moids.  */
  a68_mode (0, "VOID", &M_VOID);
  /* Standard precision.  */
  a68_mode (0, "INT", &M_INT);
  a68_mode (0, "REAL", &M_REAL);
  a68_mode (0, "COMPL", &M_COMPLEX);
  a68_mode (0, "BITS", &M_BITS);
  a68_mode (0, "BYTES", &M_BYTES);
  /* Multiple precision.  */
  a68_mode (-2, "INT", &M_SHORT_SHORT_INT);
  a68_mode (-2, "BITS", &M_SHORT_SHORT_BITS);
  a68_mode (-1, "INT", &M_SHORT_INT);
  a68_mode (-1, "BITS", &M_SHORT_BITS);
  a68_mode (1, "INT", &M_LONG_INT);
  a68_mode (1, "REAL", &M_LONG_REAL);
  a68_mode (1, "COMPL", &M_LONG_COMPLEX);
  a68_mode (1, "BITS", &M_LONG_BITS);
  a68_mode (1, "BYTES", &M_LONG_BYTES);
  a68_mode (2, "REAL", &M_LONG_LONG_REAL);
  a68_mode (2, "INT", &M_LONG_LONG_INT);
  a68_mode (2, "BITS", &M_LONG_LONG_BITS);
  a68_mode (2, "COMPL", &M_LONG_LONG_COMPLEX);
  /* Other.  */
  a68_mode (0, "BOOL", &M_BOOL);
  a68_mode (0, "CHAR", &M_CHAR);
  a68_mode (0, "STRING", &M_STRING);
  a68_mode (0, "FILE", &M_FILE);
  a68_mode (0, "CHANNEL", &M_CHANNEL);
  a68_mode (0, "SEMA", &M_SEMA);
  /* Rows.  */
  M_ROWS = a68_add_mode (&TOP_MOID (&A68_JOB), ROWS_SYMBOL, 0, NO_NODE, NO_MOID, NO_PACK);
  /* REFs.  */
  M_REF_INT = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_INT, NO_PACK);
  M_REF_REAL = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_REAL, NO_PACK);
  M_REF_COMPLEX = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_COMPLEX, NO_PACK);
  M_REF_BITS = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_BITS, NO_PACK);
  M_REF_BYTES = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_BYTES, NO_PACK);
  /* Multiple precision.  */
  M_REF_LONG_INT = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_LONG_INT, NO_PACK);
  M_REF_LONG_REAL = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_LONG_REAL, NO_PACK);
  M_REF_LONG_COMPLEX = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_LONG_COMPLEX, NO_PACK);
  M_REF_LONG_LONG_INT = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_LONG_LONG_INT, NO_PACK);
  M_REF_LONG_LONG_REAL = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_LONG_LONG_REAL, NO_PACK);
  M_REF_LONG_LONG_COMPLEX = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_LONG_LONG_COMPLEX, NO_PACK);
  M_REF_LONG_BITS = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_LONG_BITS, NO_PACK);
  M_REF_LONG_LONG_BITS = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_LONG_LONG_BITS, NO_PACK);
  M_REF_LONG_BYTES = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_LONG_BYTES, NO_PACK);
  M_REF_SHORT_INT = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_SHORT_INT, NO_PACK);
  M_REF_SHORT_SHORT_INT = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_SHORT_SHORT_INT, NO_PACK
);
  M_REF_SHORT_BITS = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_SHORT_BITS, NO_PACK);
  M_REF_SHORT_SHORT_BITS = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_SHORT_SHORT_BITS, NO_PACK);
  /* Other.  */
  M_REF_BOOL = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_BOOL, NO_PACK);
  M_REF_CHAR = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_CHAR, NO_PACK);
  M_REF_FILE = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_FILE, NO_PACK);
  M_REF_REF_FILE = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_REF_FILE, NO_PACK);
  /* [] INT.  */
  M_ROW_INT = a68_add_mode (&TOP_MOID (&A68_JOB), ROW_SYMBOL, 1, NO_NODE, M_INT, NO_PACK);
  HAS_ROWS (M_ROW_INT) = true;
  SLICE (M_ROW_INT) = M_INT;
  M_REF_ROW_INT = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_ROW_INT, NO_PACK);
  NAME (M_REF_ROW_INT) = M_REF_INT;
  /* [] REAL.  */
  M_ROW_REAL = a68_add_mode (&TOP_MOID (&A68_JOB), ROW_SYMBOL, 1, NO_NODE, M_REAL, NO_PACK);
  HAS_ROWS (M_ROW_REAL) = true;
  SLICE (M_ROW_REAL) = M_REAL;
  M_REF_ROW_REAL = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_ROW_REAL, NO_PACK);
  NAME (M_REF_ROW_REAL) = M_REF_REAL;
  /* [,] REAL.  */
  M_ROW_ROW_REAL = a68_add_mode (&TOP_MOID (&A68_JOB), ROW_SYMBOL, 2, NO_NODE, M_REAL, NO_PACK);
  HAS_ROWS (M_ROW_ROW_REAL) = true;
  SLICE (M_ROW_ROW_REAL) = M_ROW_REAL;
  M_REF_ROW_ROW_REAL = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_ROW_ROW_REAL, NO_PACK);
  NAME (M_REF_ROW_ROW_REAL) = M_REF_ROW_REAL;
  /* [] COMPLEX.  */
  M_ROW_COMPLEX = a68_add_mode (&TOP_MOID (&A68_JOB), ROW_SYMBOL, 1, NO_NODE, M_COMPLEX, NO_PACK);
  HAS_ROWS (M_ROW_COMPLEX) = true;
  SLICE (M_ROW_COMPLEX) = M_COMPLEX;
  M_REF_ROW_COMPLEX = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_ROW_COMPLEX, NO_PACK);
  NAME (M_REF_ROW_COMPLEX) = M_REF_COMPLEX;
  /* [,] COMPLEX.  */
  M_ROW_ROW_COMPLEX = a68_add_mode (&TOP_MOID (&A68_JOB), ROW_SYMBOL, 2, NO_NODE, M_COMPLEX, NO_PACK);
  HAS_ROWS (M_ROW_ROW_COMPLEX) = true;
  SLICE (M_ROW_ROW_COMPLEX) = M_ROW_COMPLEX;
  M_REF_ROW_ROW_COMPLEX = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_ROW_ROW_COMPLEX, NO_PACK);
  NAME (M_REF_ROW_ROW_COMPLEX) = M_REF_ROW_COMPLEX;
  /* [] BOOL.  */
  M_ROW_BOOL = a68_add_mode (&TOP_MOID (&A68_JOB), ROW_SYMBOL, 1, NO_NODE, M_BOOL, NO_PACK);
  HAS_ROWS (M_ROW_BOOL) = true;
  SLICE (M_ROW_BOOL) = M_BOOL;
  /* FLEX [] BOOL.  */
  MOID_T *m = a68_add_mode (&TOP_MOID (&A68_JOB), FLEX_SYMBOL, 0, NO_NODE, M_ROW_BOOL, NO_PACK);
  HAS_ROWS (m) = true;
  M_FLEX_ROW_BOOL = m;
  /* [] BITS.  */
  M_ROW_BITS = a68_add_mode (&TOP_MOID (&A68_JOB), ROW_SYMBOL, 1, NO_NODE, M_BITS, NO_PACK);
  HAS_ROWS (M_ROW_BITS) = true;
  SLICE (M_ROW_BITS) = M_BITS;
  /* [] CHAR.  */
  M_ROW_CHAR = a68_add_mode (&TOP_MOID (&A68_JOB), ROW_SYMBOL, 1, NO_NODE, M_CHAR, NO_PACK);
  HAS_ROWS (M_ROW_CHAR) = true;
  SLICE (M_ROW_CHAR) = M_CHAR;
  /* [][] CHAR.  */
  M_ROW_ROW_CHAR = a68_add_mode (&TOP_MOID (&A68_JOB), ROW_SYMBOL, 1, NO_NODE, M_ROW_CHAR, NO_PACK);
  HAS_ROWS (M_ROW_ROW_CHAR) = true;
  SLICE (M_ROW_ROW_CHAR) = M_ROW_CHAR;
  /* MODE STRING = FLEX [] CHAR.  */
  m = a68_add_mode (&TOP_MOID (&A68_JOB), FLEX_SYMBOL, 0, NO_NODE, M_ROW_CHAR, NO_PACK);
  HAS_ROWS (m) = true;
  M_FLEX_ROW_CHAR = m;
  EQUIVALENT (M_STRING) = m;
  /* REF [] CHAR.  */
  M_REF_ROW_CHAR = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, M_ROW_CHAR, NO_PACK);
  NAME (M_REF_ROW_CHAR) = M_REF_CHAR;
  /* PROC [] CHAR.  */
  M_PROC_ROW_CHAR = a68_add_mode (&TOP_MOID (&A68_JOB), PROC_SYMBOL, 0, NO_NODE, M_ROW_CHAR, NO_PACK);
  /* REF STRING = REF FLEX [] CHAR.  */
  M_REF_STRING = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, EQUIVALENT (M_STRING), NO_PACK);
  NAME (M_REF_STRING) = M_REF_CHAR;
  DEFLEXED (M_REF_STRING) = M_REF_ROW_CHAR;
  /* [] STRING.  */
  M_ROW_STRING = a68_add_mode (&TOP_MOID (&A68_JOB), ROW_SYMBOL, 1, NO_NODE, M_STRING, NO_PACK);
  HAS_ROWS (M_ROW_STRING) = true;
  SLICE (M_ROW_STRING) = M_STRING;
  DEFLEXED (M_ROW_STRING) = M_ROW_ROW_CHAR;
  /* PROC STRING.  */
  M_PROC_STRING = a68_add_mode (&TOP_MOID (&A68_JOB), PROC_SYMBOL, 0, NO_NODE, M_STRING, NO_PACK);
  DEFLEXED (M_PROC_STRING) = M_PROC_ROW_CHAR;
  /* COMPLEX.  */
  PACK_T *z = NO_PACK;
  (void) a68_add_mode_to_pack (&z, M_REAL, TEXT (a68_add_token (&A68 (top_token), "im")), NO_NODE);
  (void) a68_add_mode_to_pack (&z, M_REAL, TEXT (a68_add_token (&A68 (top_token), "re")), NO_NODE);
  m = a68_add_mode (&TOP_MOID (&A68_JOB), STRUCT_SYMBOL, a68_count_pack_members (z), NO_NODE, NO_MOID, z);
  EQUIVALENT (M_COMPLEX) = m;
  z = NO_PACK;
  (void) a68_add_mode_to_pack (&z, M_REF_REAL, TEXT (a68_add_token (&A68 (top_token), "im")), NO_NODE);
  (void) a68_add_mode_to_pack (&z, M_REF_REAL, TEXT (a68_add_token (&A68 (top_token), "re")), NO_NODE);
  m = a68_add_mode (&TOP_MOID (&A68_JOB), STRUCT_SYMBOL, a68_count_pack_members (z), NO_NODE, NO_MOID, z);
  NAME (M_REF_COMPLEX) = m;
  /* LONG COMPLEX.  */
  z = NO_PACK;
  (void) a68_add_mode_to_pack (&z, M_LONG_REAL, TEXT (a68_add_token (&A68 (top_token), "im")), NO_NODE);
  (void) a68_add_mode_to_pack (&z, M_LONG_REAL, TEXT (a68_add_token (&A68 (top_token), "re")), NO_NODE);
  m = a68_add_mode (&TOP_MOID (&A68_JOB), STRUCT_SYMBOL, a68_count_pack_members (z), NO_NODE, NO_MOID, z);
  EQUIVALENT (M_LONG_COMPLEX) = m;
  z = NO_PACK;
  (void) a68_add_mode_to_pack (&z, M_REF_LONG_REAL, TEXT (a68_add_token (&A68 (top_token), "im")), NO_NODE);
  (void) a68_add_mode_to_pack (&z, M_REF_LONG_REAL, TEXT (a68_add_token (&A68 (top_token), "re")), NO_NODE);
  m = a68_add_mode (&TOP_MOID (&A68_JOB), STRUCT_SYMBOL, a68_count_pack_members (z), NO_NODE, NO_MOID, z);
  NAME (M_REF_LONG_COMPLEX) = m;
  /* LONG_LONG COMPLEX.  */
  z = NO_PACK;
  (void) a68_add_mode_to_pack (&z, M_LONG_LONG_REAL, TEXT (a68_add_token (&A68 (top_token), "im")), NO_NODE);
  (void) a68_add_mode_to_pack (&z, M_LONG_LONG_REAL, TEXT (a68_add_token (&A68 (top_token), "re")), NO_NODE);
  m = a68_add_mode (&TOP_MOID (&A68_JOB), STRUCT_SYMBOL, a68_count_pack_members (z), NO_NODE, NO_MOID, z);
  EQUIVALENT (M_LONG_LONG_COMPLEX) = m;
  z = NO_PACK;
  (void) a68_add_mode_to_pack (&z, M_REF_LONG_LONG_REAL, TEXT (a68_add_token (&A68 (top_token), "im")), NO_NODE);
  (void) a68_add_mode_to_pack (&z, M_REF_LONG_LONG_REAL, TEXT (a68_add_token (&A68 (top_token), "re")), NO_NODE);
  m = a68_add_mode (&TOP_MOID (&A68_JOB), STRUCT_SYMBOL, a68_count_pack_members (z), NO_NODE, NO_MOID, z);
  NAME (M_REF_LONG_LONG_COMPLEX) = m;
  /* SEMA.  */
  z = NO_PACK;
  (void) a68_add_mode_to_pack (&z, M_REF_INT, NO_TEXT, NO_NODE);
  EQUIVALENT (M_SEMA) = a68_add_mode (&TOP_MOID (&A68_JOB), STRUCT_SYMBOL, a68_count_pack_members (z), NO_NODE, NO_MOID, z);
  /* PROC VOID.  */
  z = NO_PACK;
  M_PROC_VOID = a68_add_mode (&TOP_MOID (&A68_JOB), PROC_SYMBOL, a68_count_pack_members (z), NO_NODE, M_VOID, z);
  /* PROC (REAL) REAL.  */
  z = NO_PACK;
  (void) a68_add_mode_to_pack (&z, M_REAL, NO_TEXT, NO_NODE);
  M_PROC_REAL_REAL = a68_add_mode (&TOP_MOID (&A68_JOB), PROC_SYMBOL, a68_count_pack_members (z), NO_NODE, M_REAL, z);
  /* PROC (LONG_REAL) LONG_REAL.  */
  z = NO_PACK;
  (void) a68_add_mode_to_pack (&z, M_LONG_REAL, NO_TEXT, NO_NODE);
  M_PROC_LONG_REAL_LONG_REAL = a68_add_mode (&TOP_MOID (&A68_JOB), PROC_SYMBOL, a68_count_pack_members (z), NO_NODE, M_LONG_REAL, z);
  /* IO: PROC (REF FILE) BOOL.  */
  z = NO_PACK;
  (void) a68_add_mode_to_pack (&z, M_REF_FILE, NO_TEXT, NO_NODE);
  M_PROC_REF_FILE_BOOL = a68_add_mode (&TOP_MOID (&A68_JOB), PROC_SYMBOL, a68_count_pack_members (z), NO_NODE, M_BOOL, z);
  /* IO: PROC (REF FILE) VOID.  */
  z = NO_PACK;
  (void) a68_add_mode_to_pack (&z, M_REF_FILE, NO_TEXT, NO_NODE);
  M_PROC_REF_FILE_VOID = a68_add_mode (&TOP_MOID (&A68_JOB), PROC_SYMBOL, a68_count_pack_members (z), NO_NODE, M_VOID, z);
  /* IO: SIMPLIN and SIMPLOUT.  */
  M_SIMPLIN = a68_add_mode (&TOP_MOID (&A68_JOB), IN_TYPE_MODE, 0, NO_NODE, NO_MOID, NO_PACK);
  M_ROW_SIMPLIN = a68_add_mode (&TOP_MOID (&A68_JOB), ROW_SYMBOL, 1, NO_NODE, M_SIMPLIN, NO_PACK);
  SLICE (M_ROW_SIMPLIN) = M_SIMPLIN;
  M_SIMPLOUT = a68_add_mode (&TOP_MOID (&A68_JOB), OUT_TYPE_MODE, 0, NO_NODE, NO_MOID, NO_PACK);
  M_ROW_SIMPLOUT = a68_add_mode (&TOP_MOID (&A68_JOB), ROW_SYMBOL, 1, NO_NODE, M_SIMPLOUT, NO_PACK);
  SLICE (M_ROW_SIMPLOUT) = M_SIMPLOUT;
}

/* Set up standenv - general RR but not transput.  */

static void
stand_prelude (void)
{
  /* Identifiers.  */
  a68_idf (A68_STD, "intlengths", M_INT, a68_lower_intlengths);
  a68_idf (A68_STD, "intshorths", M_INT, a68_lower_intshorths);
  a68_idf (A68_STD, "maxint", M_INT, a68_lower_maxint);
  a68_idf (A68_STD, "longmaxint", M_LONG_INT, a68_lower_maxint);
  a68_idf (A68_STD, "longlongmaxint", M_LONG_LONG_INT, a68_lower_maxint);
  a68_idf (A68_STD, "shortmaxint", M_SHORT_INT, a68_lower_maxint);
  a68_idf (A68_STD, "shortshortmaxint", M_SHORT_SHORT_INT, a68_lower_maxint);
  a68_idf (A68_STD, "maxreal", M_REAL, a68_lower_maxreal);
  a68_idf (A68_STD, "longmaxreal", M_LONG_REAL, a68_lower_maxreal);
  a68_idf (A68_STD, "longlongmaxreal", M_LONG_LONG_REAL, a68_lower_maxreal);
  a68_idf (A68_STD, "smallreal", M_REAL, a68_lower_smallreal);
  a68_idf (A68_STD, "longsmallreal", M_LONG_REAL, a68_lower_smallreal);
  a68_idf (A68_STD, "longlongsmallreal", M_LONG_LONG_REAL, a68_lower_smallreal);
  a68_idf (A68_STD, "reallengths", M_INT, a68_lower_reallengths);
  a68_idf (A68_STD, "realshorths", M_INT, a68_lower_realshorths);
  a68_idf (A68_STD, "bitslengths", M_INT, a68_lower_bitslengths);
  a68_idf (A68_STD, "bitsshorths", M_INT, a68_lower_bitsshorths);
  a68_idf (A68_STD, "bitswidth", M_INT, a68_lower_bitswidth);
  a68_idf (A68_STD, "longbitswidth", M_INT, a68_lower_longbitswidth);
  a68_idf (A68_STD, "longlongbitswidth", M_INT, a68_lower_longlongbitswidth);
  a68_idf (A68_STD, "shortbitswidth", M_INT, a68_lower_shortbitswidth);
  a68_idf (A68_STD, "shortshortbitswidth", M_INT, a68_lower_shortshortbitswidth);
  a68_idf (A68_STD, "maxbits", M_BITS, a68_lower_maxbits);
  a68_idf (A68_STD, "longmaxbits", M_LONG_BITS, a68_lower_maxbits);
  a68_idf (A68_STD, "longlongmaxbits", M_LONG_LONG_BITS, a68_lower_maxbits);
  a68_idf (A68_STD, "maxabschar", M_INT, a68_lower_maxabschar);
  a68_idf (A68_STD, "intwidth", M_INT, a68_lower_intwidth);
  a68_idf (A68_STD, "longintwidth", M_INT, a68_lower_longintwidth);
  a68_idf (A68_STD, "longlongintwidth", M_INT, a68_lower_longlongintwidth);
  a68_idf (A68_STD, "shortintwidth", M_INT, a68_lower_shortintwidth);
  a68_idf (A68_STD, "shortshortintwidth", M_INT, a68_lower_shortshortintwidth);
  a68_idf (A68_STD, "realwidth", M_INT, a68_lower_realwidth);
  a68_idf (A68_STD, "longrealwidth", M_INT, a68_lower_longrealwidth);
  a68_idf (A68_STD, "longlongrealwidth", M_INT, a68_lower_longlongrealwidth);
  a68_idf (A68_STD, "expwidth", M_INT, a68_lower_expwidth);
  a68_idf (A68_STD, "longexpwidth", M_INT, a68_lower_longexpwidth);
  a68_idf (A68_STD, "longlongexpwidth", M_INT, a68_lower_longlongexpwidth);
  a68_idf (A68_STD, "pi", M_REAL, a68_lower_pi);
  a68_idf (A68_STD, "longpi", M_LONG_REAL, a68_lower_pi);
  a68_idf (A68_STD, "longlongpi", M_LONG_LONG_REAL, a68_lower_pi);
  a68_idf (A68_STD, "compllengths", M_INT);
  a68_idf (A68_STD, "complshorths", M_INT);
  a68_idf (A68_STD, "byteslengths", M_INT);
  a68_idf (A68_STD, "bytesshorths", M_INT);
  a68_idf (A68_STD, "byteswidth", M_INT);
  a68_idf (A68_STD, "longbyteswidth", M_INT);
  a68_idf (A68_STD, "flip", M_CHAR, a68_lower_flip);
  a68_idf (A68_STD, "flop", M_CHAR, a68_lower_flop);
  a68_idf (A68_STD, "errorchar", M_CHAR, a68_lower_errorchar);
  a68_idf (A68_STD, "nullcharacter", M_CHAR, a68_lower_nullcharacter);
  a68_idf (A68_STD, "blank", M_CHAR, a68_lower_blank);
  /* BITS procedures.  */
  MOID_T *m = a68_proc (M_BITS, M_ROW_BOOL, NO_MOID);
  a68_idf (A68_STD, "bitspack", m);
  /* SHORT BITS procedures.  */
  m = a68_proc (M_SHORT_BITS, M_ROW_BOOL, NO_MOID);
  a68_idf (A68_STD, "shortbitspack", m);
  /* SHORT SHORT BITS procedures.  */
  m = a68_proc (M_SHORT_SHORT_BITS, M_ROW_BOOL, NO_MOID);
  a68_idf (A68_STD, "shortshortbitspack", m);
  /* LONG BITS procedures.  */
  m = a68_proc (M_LONG_BITS, M_ROW_BOOL, NO_MOID);
  a68_idf (A68_STD, "longbitspack", m);
  /* LONG LONG BITS procedures.  */
  m = a68_proc (M_LONG_LONG_BITS, M_ROW_BOOL, NO_MOID);
  a68_idf (A68_STD, "longlongbitspack", m);
  /* RNG procedures.  */
  m = a68_proc (M_VOID, M_INT, NO_MOID);
  a68_idf (A68_STD, "firstrandom", m);
  /* REAL procedures.  */
  m = A68_MCACHE (proc_real);
  a68_idf (A68_STD, "nextrandom", m);
  a68_idf (A68_STD, "random", m, a68_lower_random);
  a68_idf (A68_STD, "rnd", m);
  m = A68_MCACHE (proc_real_real);
  a68_idf (A68_STD, "arccos", m, a68_lower_acos);
  a68_idf (A68_STD, "arcsin", m, a68_lower_asin);
  a68_idf (A68_STD, "arctan", m, a68_lower_atan);
  a68_idf (A68_STD, "cos", m, a68_lower_cos);
  a68_idf (A68_STD, "exp", m, a68_lower_exp);
  a68_idf (A68_STD, "ln", m, a68_lower_ln);
  a68_idf (A68_STD, "sin", m, a68_lower_sin);
  a68_idf (A68_STD, "sqrt", m, a68_lower_sqrt);
  a68_idf (A68_STD, "tan", m, a68_lower_tan);
  /* LONG REAL procedures.  */
  m = a68_proc (M_LONG_REAL, NO_MOID);
  a68_idf (A68_STD, "longnextrandom", m);
  a68_idf (A68_STD, "longrandom", m, a68_lower_longrandom);
  m = a68_proc (M_LONG_REAL, M_LONG_REAL, NO_MOID);
  a68_idf (A68_STD, "longarccos", m, a68_lower_long_acos);
  a68_idf (A68_STD, "longarcsin", m, a68_lower_long_asin);
  a68_idf (A68_STD, "longarctan", m, a68_lower_long_atan);
  a68_idf (A68_STD, "longcos", m, a68_lower_long_cos);
  a68_idf (A68_STD, "longexp", m, a68_lower_long_exp);
  a68_idf (A68_STD, "longln", m, a68_lower_long_ln);
  a68_idf (A68_STD, "longsin", m, a68_lower_long_sin);
  a68_idf (A68_STD, "longsqrt", m, a68_lower_long_sqrt);
  a68_idf (A68_STD, "longtan", m, a68_lower_long_tan);
  /* LONG LONG REAL procedures.  */
  m = a68_proc (M_LONG_LONG_REAL, NO_MOID);
  a68_idf (A68_STD, "longlongnextrandom", m);
  a68_idf (A68_STD, "longlongrandom", m, a68_lower_longlongrandom);
  m = a68_proc (M_LONG_LONG_REAL, M_LONG_LONG_REAL, NO_MOID);
  a68_idf (A68_STD, "longlongarccos", m, a68_lower_long_long_acos);
  a68_idf (A68_STD, "longlongarcsin", m, a68_lower_long_long_asin);
  a68_idf (A68_STD, "longlongarctan", m, a68_lower_long_long_atan);
  a68_idf (A68_STD, "longlongcos", m, a68_lower_long_long_cos);
  a68_idf (A68_STD, "longlongexp", m, a68_lower_long_long_exp);
  a68_idf (A68_STD, "longlongln", m, a68_lower_long_long_ln);
  a68_idf (A68_STD, "longlongsin", m, a68_lower_long_long_sin);
  a68_idf (A68_STD, "longlongsqrt", m, a68_lower_long_long_sqrt);
  a68_idf (A68_STD, "longlongtan", m, a68_lower_long_long_tan);
  /* Priorities.  */
  a68_prio ("+:=", 1);
  a68_prio ("-:=", 1);
  a68_prio ("*:=", 1);
  a68_prio ("/:=", 1);
  a68_prio ("%:=", 1);
  a68_prio ("%*:=", 1);
  a68_prio ("+=:", 1);
  a68_prio ("PLUSAB", 1);
  a68_prio ("MINUSAB", 1);
  a68_prio ("TIMESAB", 1);
  a68_prio ("DIVAB", 1);
  a68_prio ("OVERAB", 1);
  a68_prio ("MODAB", 1);
  a68_prio ("PLUSTO", 1);
  a68_prio ("OR", 2);
  a68_prio ("AND", 3);
  a68_prio ("XOR", 3);
  a68_prio ("=", 4);
  a68_prio ("/=", 4);
  a68_prio ("<", 5);
  a68_prio ("<=", 5);
  a68_prio (">", 5);
  a68_prio (">=", 5);
  a68_prio ("EQ", 4);
  a68_prio ("NE", 4);
  a68_prio ("LT", 5);
  a68_prio ("LE", 5);
  a68_prio ("GT", 5);
  a68_prio ("GE", 5);
  a68_prio ("+", 6);
  a68_prio ("-", 6);
  a68_prio ("*", 7);
  a68_prio ("/", 7);
  a68_prio ("OVER", 7);
  a68_prio ("%", 7);
  a68_prio ("MOD", 7);
  a68_prio ("%*", 7);
  a68_prio ("ELEM", 7);
  a68_prio ("**", 8);
  a68_prio ("SHL", 8);
  a68_prio ("SHR", 8);
  a68_prio ("UP", 8);
  a68_prio ("DOWN", 8);
  a68_prio ("LWB", 8);
  a68_prio ("UPB", 8);
  a68_prio ("I", 9);
  a68_prio ("+*", 9);
  /* BOOL operators.  */
  m = a68_proc (M_BOOL, M_BOOL, NO_MOID);
  a68_op (A68_STD, "NOT", m, a68_lower_not2);
  a68_op (A68_STD, "~", m, a68_lower_not2);
  m = a68_proc (M_INT, M_BOOL, NO_MOID);
  a68_op (A68_STD, "ABS", m, a68_lower_boolabs2);
  m = a68_proc (M_BOOL, M_BOOL, M_BOOL, NO_MOID);
  a68_op (A68_STD, "OR", m, a68_lower_or3);
  a68_op (A68_STD, "AND", m, a68_lower_and3);
  a68_op (A68_STD, "=", m, a68_lower_bool_eq3);
  a68_op (A68_STD, "/=", m, a68_lower_bool_ne3);
  a68_op (A68_STD, "EQ", m, a68_lower_bool_eq3);
  a68_op (A68_STD, "NE", m, a68_lower_bool_ne3);
  /* CHAR operators.  */
  m = a68_proc (M_BOOL, M_CHAR, M_CHAR, NO_MOID);
  a68_op (A68_STD, "=", m, a68_lower_char_eq3);
  a68_op (A68_STD, "/=", m, a68_lower_char_ne3);
  a68_op (A68_STD, "<", m, a68_lower_char_lt3);
  a68_op (A68_STD, "<=", m, a68_lower_char_le3);
  a68_op (A68_STD, ">", m, a68_lower_char_gt3);
  a68_op (A68_STD, ">=", m, a68_lower_char_ge3);
  a68_op (A68_STD, "EQ", m, a68_lower_char_eq3);
  a68_op (A68_STD, "NE", m, a68_lower_char_ne3);
  a68_op (A68_STD, "LT", m, a68_lower_char_lt3);
  a68_op (A68_STD, "LE", m, a68_lower_char_le3);
  a68_op (A68_STD, "GT", m, a68_lower_char_gt3);
  a68_op (A68_STD, "GE", m, a68_lower_char_ge3);
  m = a68_proc (M_INT, M_CHAR, NO_MOID);
  a68_op (A68_STD, "ABS", m, a68_lower_charabs2);
  m = a68_proc (M_CHAR, M_INT, NO_MOID);
  a68_op (A68_STD, "REPR", m, a68_lower_repr2);
  /* STRING operators.  */
  m = a68_proc (M_BOOL, M_STRING, M_STRING, NO_MOID);
  a68_op (A68_STD, "=", m, a68_lower_string_eq3);
  a68_op (A68_STD, "/=", m, a68_lower_string_ne3);
  a68_op (A68_STD, "<", m, a68_lower_string_lt3);
  a68_op (A68_STD, "<=", m, a68_lower_string_le3);
  a68_op (A68_STD, ">=", m, a68_lower_string_ge3);
  a68_op (A68_STD, ">", m, a68_lower_string_gt3);
  a68_op (A68_STD, "EQ", m, a68_lower_string_eq3);
  a68_op (A68_STD, "NE", m, a68_lower_string_ne3);
  a68_op (A68_STD, "LT", m, a68_lower_string_lt3);
  a68_op (A68_STD, "LE", m, a68_lower_string_le3);
  a68_op (A68_STD, "GE", m, a68_lower_string_ge3);
  a68_op (A68_STD, "GT", m, a68_lower_string_gt3);
  m = a68_proc (M_STRING, M_CHAR, M_CHAR, NO_MOID);
  a68_op (A68_STD, "+", m, a68_lower_char_plus3);
  m = a68_proc (M_STRING, M_STRING, M_STRING, NO_MOID);
  a68_op (A68_STD, "+", m, a68_lower_string_plus3);
  m = a68_proc (M_REF_STRING, M_REF_STRING, M_STRING, NO_MOID);
  a68_op (A68_STD, "+:=", m, a68_lower_string_plusab3);
  a68_op (A68_STD, "PLUSAB", m, a68_lower_string_plusab3);
  m = a68_proc (M_REF_STRING, M_REF_STRING, M_INT, NO_MOID);
  a68_op (A68_STD, "*:=", m, a68_lower_string_multab3);
  a68_op (A68_STD, "TIMESAB", m, a68_lower_string_multab3);
  m = a68_proc (M_REF_STRING, M_STRING, M_REF_STRING, NO_MOID);
  a68_op (A68_STD, "+=:", m, a68_lower_string_plusto3);
  a68_op (A68_STD, "PLUSTO", m, a68_lower_string_plusto3);
  m = a68_proc (M_STRING, M_STRING, M_INT, NO_MOID);
  a68_op (A68_STD, "*", m, a68_lower_string_mult3);
  m = a68_proc (M_STRING, M_INT, M_STRING, NO_MOID);
  a68_op (A68_STD, "*", m, a68_lower_string_mult3);
  m = a68_proc (M_STRING, M_INT, M_CHAR, NO_MOID);
  a68_op (A68_STD, "*", m, a68_lower_char_mult3);
  m = a68_proc (M_STRING, M_CHAR, M_INT, NO_MOID);
  a68_op (A68_STD, "*", m, a68_lower_char_mult3);
  /* SHORT SHORT INT operators.  */
  m = a68_proc (M_SHORT_SHORT_INT, M_SHORT_SHORT_INT, NO_MOID);
  a68_op (A68_STD, "+", m, a68_lower_confirm2);
  a68_op (A68_STD, "-", m, a68_lower_negate2);
  a68_op (A68_STD, "ABS", m, a68_lower_intabs2);
  m = a68_proc (M_INT, M_SHORT_SHORT_INT, NO_MOID);
  a68_op (A68_STD, "SIGN", m, a68_lower_sign2);
  m = a68_proc (M_SHORT_INT, M_SHORT_SHORT_INT, NO_MOID);
  a68_op (A68_STD, "LENG", m, a68_lower_lengint2);
  m = a68_proc (M_BOOL, M_SHORT_SHORT_INT, NO_MOID);
  a68_op (A68_STD, "ODD", m, a68_lower_odd2);
  m = a68_proc (M_SHORT_SHORT_INT, M_SHORT_SHORT_INT, M_SHORT_SHORT_INT, NO_MOID);
  a68_op (A68_STD, "+", m, a68_lower_plus_int);
  a68_op (A68_STD, "-", m, a68_lower_minus_int);
  a68_op (A68_STD, "*", m, a68_lower_mult_int);
  a68_op (A68_STD, "OVER", m, a68_lower_over3);
  a68_op (A68_STD, "%", m, a68_lower_over3);
  a68_op (A68_STD, "MOD", m, a68_lower_mod3);
  a68_op (A68_STD, "%*", m, a68_lower_mod3);
  m = a68_proc (M_REF_SHORT_SHORT_INT, M_REF_SHORT_SHORT_INT, M_SHORT_SHORT_INT, NO_MOID);
  a68_op (A68_STD, "+:=", m, a68_lower_plusab3);
  a68_op (A68_STD, "-:=", m, a68_lower_minusab3);
  a68_op (A68_STD, "*:=", m, a68_lower_multab3);
  a68_op (A68_STD, "%:=", m, a68_lower_overab3);
  a68_op (A68_STD, "%*:=", m, a68_lower_modab3);
  a68_op (A68_STD, "PLUSAB", m, a68_lower_plusab3);
  a68_op (A68_STD, "MINUSAB", m, a68_lower_minusab3);
  a68_op (A68_STD, "TIMESAB", m, a68_lower_multab3);
  a68_op (A68_STD, "OVERAB", m, a68_lower_overab3);
  a68_op (A68_STD, "MODAB", m, a68_lower_modab3);
  m = a68_proc (M_BOOL, M_SHORT_SHORT_INT, M_SHORT_SHORT_INT, NO_MOID);
  a68_op (A68_STD, "EQ", m, a68_lower_int_eq3);
  a68_op (A68_STD, "NE", m, a68_lower_int_ne3);
  a68_op (A68_STD, "GE", m, a68_lower_int_ge3);
  a68_op (A68_STD, "GT", m, a68_lower_int_gt3);
  a68_op (A68_STD, "LE", m, a68_lower_int_le3);
  a68_op (A68_STD, "LT", m, a68_lower_int_lt3);
  a68_op (A68_STD, "=", m, a68_lower_int_eq3);
  a68_op (A68_STD, ">=", m, a68_lower_int_ge3);
  a68_op (A68_STD, ">", m, a68_lower_int_gt3);
  a68_op (A68_STD, "<=", m, a68_lower_int_le3);
  a68_op (A68_STD, "<", m, a68_lower_int_lt3);
  a68_op (A68_STD, "/=", m, a68_lower_int_ne3);
  m = a68_proc (M_SHORT_SHORT_INT, M_SHORT_SHORT_INT, M_INT, NO_MOID);
  a68_op (A68_STD, "**", m, a68_lower_pow_int);
  /* SHORT INT operators.  */
  m = a68_proc (M_SHORT_INT, M_SHORT_INT, NO_MOID);
  a68_op (A68_STD, "+", m, a68_lower_confirm2);
  a68_op (A68_STD, "-", m, a68_lower_negate2);
  a68_op (A68_STD, "ABS", m, a68_lower_intabs2);
  m = a68_proc (M_SHORT_SHORT_INT, M_SHORT_INT, NO_MOID);
  a68_op (A68_STD, "SHORTEN", m, a68_lower_shortenint2);
  m = a68_proc (M_INT, M_SHORT_INT, NO_MOID);
  a68_op (A68_STD, "LENG", m, a68_lower_lengint2);
  a68_op (A68_STD, "SIGN", m, a68_lower_sign2);
  m = a68_proc (M_BOOL, M_SHORT_INT, NO_MOID);
  a68_op (A68_STD, "ODD", m, a68_lower_odd2);
  m = a68_proc (M_SHORT_INT, M_SHORT_INT, M_SHORT_INT, NO_MOID);
  a68_op (A68_STD, "+", m, a68_lower_plus_int);
  a68_op (A68_STD, "-", m, a68_lower_minus_int);
  a68_op (A68_STD, "*", m, a68_lower_mult_int);
  a68_op (A68_STD, "OVER", m, a68_lower_over3);
  a68_op (A68_STD, "%", m, a68_lower_over3);
  a68_op (A68_STD, "MOD", m, a68_lower_mod3);
  a68_op (A68_STD, "%*", m, a68_lower_mod3);
  m = a68_proc (M_REF_SHORT_INT, M_REF_SHORT_INT, M_SHORT_INT, NO_MOID);
  a68_op (A68_STD, "+:=", m, a68_lower_plusab3);
  a68_op (A68_STD, "-:=", m, a68_lower_minusab3);
  a68_op (A68_STD, "*:=", m, a68_lower_multab3);
  a68_op (A68_STD, "%:=", m, a68_lower_overab3);
  a68_op (A68_STD, "%*:=", m, a68_lower_modab3);
  a68_op (A68_STD, "PLUSAB", m, a68_lower_plusab3);
  a68_op (A68_STD, "MINUSAB", m, a68_lower_minusab3);
  a68_op (A68_STD, "TIMESAB", m, a68_lower_multab3);
  a68_op (A68_STD, "OVERAB", m, a68_lower_overab3);
  a68_op (A68_STD, "MODAB", m, a68_lower_modab3);
  m = a68_proc (M_BOOL, M_SHORT_INT, M_SHORT_INT, NO_MOID);
  a68_op (A68_STD, "=", m, a68_lower_int_eq3);
  a68_op (A68_STD, "EQ", m, a68_lower_int_eq3);
  a68_op (A68_STD, "/=", m, a68_lower_int_ne3);
  a68_op (A68_STD, "NE", m, a68_lower_int_ne3);
  a68_op (A68_STD, "<", m, a68_lower_int_lt3);
  a68_op (A68_STD, "LT", m, a68_lower_int_lt3);
  a68_op (A68_STD, "<=", m, a68_lower_int_le3);
  a68_op (A68_STD, "LE", m, a68_lower_int_le3);
  a68_op (A68_STD, ">", m, a68_lower_int_gt3);
  a68_op (A68_STD, "GT", m, a68_lower_int_gt3);
  a68_op (A68_STD, ">=", m, a68_lower_int_ge3);
  a68_op (A68_STD, "GE", m, a68_lower_int_ge3);
  m = a68_proc (M_SHORT_INT, M_SHORT_INT, M_INT, NO_MOID);
  a68_op (A68_STD, "**", m, a68_lower_pow_int);
  /* INT operators. */
  m = a68_proc (M_INT, M_INT, NO_MOID);
  a68_op (A68_STD, "+", m, a68_lower_confirm2);
  a68_op (A68_STD, "-", m, a68_lower_negate2);
  a68_op (A68_STD, "ABS", m, a68_lower_intabs2);
  a68_op (A68_STD, "SIGN", m, a68_lower_sign2);
  m = a68_proc (M_SHORT_INT, M_INT, NO_MOID);
  a68_op (A68_STD, "SHORTEN", m, a68_lower_shortenint2);
  m = a68_proc (M_LONG_INT, M_INT, NO_MOID);
  a68_op (A68_STD, "LENG", m, a68_lower_lengint2);
  m = a68_proc (M_BOOL, M_INT, NO_MOID);
  a68_op (A68_STD, "ODD", m, a68_lower_odd2);
  m = a68_proc (M_BOOL, M_INT, M_INT, NO_MOID);
  a68_op (A68_STD, "=", m, a68_lower_int_eq3);
  a68_op (A68_STD, "/=", m, a68_lower_int_ne3);
  a68_op (A68_STD, "<", m, a68_lower_int_lt3);
  a68_op (A68_STD, "<=", m, a68_lower_int_le3);
  a68_op (A68_STD, ">", m, a68_lower_int_gt3);
  a68_op (A68_STD, ">=", m, a68_lower_int_ge3);
  a68_op (A68_STD, "EQ", m, a68_lower_int_eq3);
  a68_op (A68_STD, "NE", m, a68_lower_int_ne3);
  a68_op (A68_STD, "LT", m, a68_lower_int_lt3);
  a68_op (A68_STD, "LE", m, a68_lower_int_le3);
  a68_op (A68_STD, "GT", m, a68_lower_int_gt3);
  a68_op (A68_STD, "GE", m, a68_lower_int_ge3);
  m = a68_proc (M_INT, M_INT, M_INT, NO_MOID);
  a68_op (A68_STD, "+", m, a68_lower_plus_int);
  a68_op (A68_STD, "-", m, a68_lower_minus_int);
  a68_op (A68_STD, "*", m, a68_lower_mult_int);
  a68_op (A68_STD, "OVER", m, a68_lower_over3);
  a68_op (A68_STD, "%", m, a68_lower_over3);
  a68_op (A68_STD, "MOD", m, a68_lower_mod3);
  a68_op (A68_STD, "%*", m, a68_lower_mod3);
  a68_op (A68_STD, "**", m, a68_lower_pow_int);
  m = a68_proc (M_REAL, M_INT, M_INT, NO_MOID);
  a68_op (A68_STD, "/", m, a68_lower_rdiv3);
  m = a68_proc (M_REF_INT, M_REF_INT, M_INT, NO_MOID);
  a68_op (A68_STD, "+:=", m, a68_lower_plusab3);
  a68_op (A68_STD, "-:=", m, a68_lower_minusab3);
  a68_op (A68_STD, "*:=", m, a68_lower_multab3);
  a68_op (A68_STD, "%:=", m, a68_lower_overab3);
  a68_op (A68_STD, "%*:=", m, a68_lower_modab3);
  a68_op (A68_STD, "PLUSAB", m, a68_lower_plusab3);
  a68_op (A68_STD, "MINUSAB", m, a68_lower_minusab3);
  a68_op (A68_STD, "TIMESAB", m, a68_lower_multab3);
  a68_op (A68_STD, "OVERAB", m, a68_lower_overab3);
  a68_op (A68_STD, "MODAB", m, a68_lower_modab3);
  /* LONG INT operators */
  m = a68_proc (M_LONG_INT, M_LONG_INT, NO_MOID);
  a68_op (A68_STD, "+", m, a68_lower_confirm2);
  a68_op (A68_STD, "-", m, a68_lower_negate2);
  a68_op (A68_STD, "ABS", m, a68_lower_intabs2);
  m = a68_proc (M_INT, M_LONG_INT, NO_MOID);
  a68_op (A68_STD, "SHORTEN", m, a68_lower_shortenint2);
  a68_op (A68_STD, "SIGN", m, a68_lower_sign2);
  m = a68_proc (M_LONG_LONG_INT, M_LONG_INT, NO_MOID);
  a68_op (A68_STD, "LENG", m, a68_lower_lengint2);
  m = a68_proc (M_BOOL, M_LONG_INT, NO_MOID);
  a68_op (A68_STD, "ODD", m, a68_lower_odd2);
  m = a68_proc (M_LONG_INT, M_LONG_INT, M_LONG_INT, NO_MOID);
  a68_op (A68_STD, "+", m, a68_lower_plus_int);
  a68_op (A68_STD, "-", m, a68_lower_minus_int);
  a68_op (A68_STD, "*", m, a68_lower_mult_int);
  a68_op (A68_STD, "OVER", m, a68_lower_over3);
  a68_op (A68_STD, "%", m, a68_lower_over3);
  a68_op (A68_STD, "MOD", m, a68_lower_mod3);
  a68_op (A68_STD, "%*", m, a68_lower_mod3);
  m = a68_proc (M_REF_LONG_INT, M_REF_LONG_INT, M_LONG_INT, NO_MOID);
  a68_op (A68_STD, "+:=", m, a68_lower_plusab3);
  a68_op (A68_STD, "-:=", m, a68_lower_minusab3);
  a68_op (A68_STD, "*:=", m, a68_lower_multab3);
  a68_op (A68_STD, "%:=", m, a68_lower_overab3);
  a68_op (A68_STD, "%*:=", m, a68_lower_modab3);
  a68_op (A68_STD, "PLUSAB", m, a68_lower_plusab3);
  a68_op (A68_STD, "MINUSAB", m, a68_lower_minusab3);
  a68_op (A68_STD, "TIMESAB", m, a68_lower_multab3);
  a68_op (A68_STD, "OVERAB", m, a68_lower_overab3);
  a68_op (A68_STD, "MODAB", m, a68_lower_modab3);
  m = a68_proc (M_BOOL, M_LONG_INT, M_LONG_INT, NO_MOID);
  a68_op (A68_STD, "=", m, a68_lower_int_eq3);
  a68_op (A68_STD, "EQ", m, a68_lower_int_eq3);
  a68_op (A68_STD, "/=", m, a68_lower_int_ne3);
  a68_op (A68_STD, "NE", m, a68_lower_int_ne3);
  a68_op (A68_STD, "<", m, a68_lower_int_lt3);
  a68_op (A68_STD, "LT", m, a68_lower_int_lt3);
  a68_op (A68_STD, "<=", m, a68_lower_int_le3);
  a68_op (A68_STD, "LE", m, a68_lower_int_le3);
  a68_op (A68_STD, ">", m, a68_lower_int_gt3);
  a68_op (A68_STD, "GT", m, a68_lower_int_gt3);
  a68_op (A68_STD, ">=", m, a68_lower_int_ge3);
  a68_op (A68_STD, "GE", m, a68_lower_int_ge3);
  m = a68_proc (M_LONG_REAL, M_LONG_INT, M_LONG_INT, NO_MOID);
  a68_op (A68_STD, "/", m, a68_lower_rdiv3);
  m = a68_proc (M_LONG_INT, M_LONG_INT, M_INT, NO_MOID);
  a68_op (A68_STD, "**", m, a68_lower_pow_int);
  /* LONG LONG INT operators. */
  m = a68_proc (M_LONG_LONG_INT, M_LONG_LONG_INT, NO_MOID);
  a68_op (A68_STD, "+", m, a68_lower_confirm2);
  a68_op (A68_STD, "-", m, a68_lower_negate2);
  a68_op (A68_STD, "ABS", m, a68_lower_intabs2);
  m = a68_proc (M_INT, M_LONG_LONG_INT, NO_MOID);
  a68_op (A68_STD, "SIGN", m, a68_lower_sign2);
  m = a68_proc (M_LONG_INT, M_LONG_LONG_INT, NO_MOID);
  a68_op (A68_STD, "SHORTEN", m, a68_lower_shortenint2);
  m = a68_proc (M_BOOL, M_LONG_LONG_INT, NO_MOID);
  a68_op (A68_STD, "ODD", m, a68_lower_odd2);
  m = a68_proc (M_LONG_LONG_INT, M_LONG_LONG_INT, M_LONG_LONG_INT, NO_MOID);
  a68_op (A68_STD, "+", m, a68_lower_plus_int);
  a68_op (A68_STD, "-", m, a68_lower_minus_int);
  a68_op (A68_STD, "*", m, a68_lower_mult_int);
  a68_op (A68_STD, "OVER", m, a68_lower_over3);
  a68_op (A68_STD, "%", m, a68_lower_over3);
  a68_op (A68_STD, "MOD", m, a68_lower_mod3);
  a68_op (A68_STD, "%*", m, a68_lower_mod3);
  m = a68_proc (M_REF_LONG_LONG_INT, M_REF_LONG_LONG_INT, M_LONG_LONG_INT, NO_MOID);
  a68_op (A68_STD, "+:=", m, a68_lower_plusab3);
  a68_op (A68_STD, "-:=", m, a68_lower_minusab3);
  a68_op (A68_STD, "*:=", m, a68_lower_multab3);
  a68_op (A68_STD, "%:=", m, a68_lower_overab3);
  a68_op (A68_STD, "%*:=", m, a68_lower_modab3);
  a68_op (A68_STD, "PLUSAB", m, a68_lower_plusab3);
  a68_op (A68_STD, "MINUSAB", m, a68_lower_minusab3);
  a68_op (A68_STD, "TIMESAB", m, a68_lower_multab3);
  a68_op (A68_STD, "OVERAB", m, a68_lower_overab3);
  a68_op (A68_STD, "MODAB", m, a68_lower_modab3);
  m = a68_proc (M_LONG_LONG_REAL, M_LONG_LONG_INT, M_LONG_LONG_INT, NO_MOID);
  a68_op (A68_STD, "/", m, a68_lower_rdiv3);
  m = a68_proc (M_BOOL, M_LONG_LONG_INT, M_LONG_LONG_INT, NO_MOID);
  a68_op (A68_STD, "EQ", m, a68_lower_int_eq3);
  a68_op (A68_STD, "NE", m, a68_lower_int_ne3);
  a68_op (A68_STD, "GE", m, a68_lower_int_ge3);
  a68_op (A68_STD, "GT", m, a68_lower_int_gt3);
  a68_op (A68_STD, "LE", m, a68_lower_int_le3);
  a68_op (A68_STD, "LT", m, a68_lower_int_lt3);
  a68_op (A68_STD, "=", m, a68_lower_int_eq3);
  a68_op (A68_STD, ">=", m, a68_lower_int_ge3);
  a68_op (A68_STD, ">", m, a68_lower_int_gt3);
  a68_op (A68_STD, "<=", m, a68_lower_int_le3);
  a68_op (A68_STD, "<", m, a68_lower_int_lt3);
  a68_op (A68_STD, "/=", m, a68_lower_int_ne3);
  m = a68_proc (M_LONG_LONG_INT, M_LONG_LONG_INT, M_INT, NO_MOID);
  a68_op (A68_STD, "**", m, a68_lower_pow_int);
  /* SHORT SHORT BITS operators  */
  m = a68_proc (M_BOOL, M_SHORT_SHORT_BITS, M_SHORT_SHORT_BITS, NO_MOID);
  a68_op (A68_STD, "=", m, a68_lower_bit_eq3);
  a68_op (A68_STD, "EQ", m, a68_lower_bit_eq3);
  a68_op (A68_STD, "/=", m, a68_lower_bit_ne3);
  a68_op (A68_STD, "NE", m, a68_lower_bit_ne3);
  a68_op (A68_STD, "<=", m, a68_lower_bit_le3);
  a68_op (A68_STD, "LE", m, a68_lower_bit_le3);
  a68_op (A68_STD, ">=", m, a68_lower_bit_ge3);
  a68_op (A68_STD, "GE", m, a68_lower_bit_ge3);
  m = a68_proc (M_SHORT_SHORT_BITS, M_SHORT_SHORT_BITS, NO_MOID);
  a68_op (A68_STD, "NOT", m, a68_lower_bitnot2);
  a68_op (A68_STD, "~", m, a68_lower_bitnot2);
  m = a68_proc (M_SHORT_SHORT_BITS, M_SHORT_SHORT_BITS, M_SHORT_SHORT_BITS, NO_MOID);
  a68_op (A68_STD, "AND", m, a68_lower_bitand3);
  a68_op (A68_STD, "OR", m, a68_lower_bitior3);
  m = a68_proc (M_SHORT_SHORT_BITS, M_SHORT_SHORT_BITS, M_INT, NO_MOID);
  a68_op (A68_STD, "SHL", m, a68_lower_shl3);
  a68_op (A68_STD, "UP", m, a68_lower_shl3);
  a68_op (A68_STD, "SHR", m, a68_lower_shr3);
  a68_op (A68_STD, "DOWN", m, a68_lower_shr3);
  m = a68_proc (M_BOOL, M_INT, M_SHORT_SHORT_BITS, NO_MOID);
  a68_op (A68_STD, "ELEM", m, a68_lower_bitelem3);
  m = a68_proc (M_SHORT_SHORT_BITS, M_SHORT_SHORT_INT, NO_MOID);
  a68_op (A68_STD, "BIN", m, a68_lower_bin2);
  m = a68_proc (M_SHORT_SHORT_INT, M_SHORT_SHORT_BITS, NO_MOID);
  a68_op (A68_STD, "ABS", m, a68_lower_bitabs2);
  m = a68_proc (M_SHORT_BITS, M_SHORT_SHORT_BITS, NO_MOID);
  a68_op (A68_STD, "LENG", m, a68_lower_bitleng2);
  /* SHORT BITS operatos.  */
  m = a68_proc (M_SHORT_INT, M_SHORT_BITS, NO_MOID);
  a68_op (A68_STD, "ABS", m, a68_lower_bitabs2);
  m = a68_proc (M_SHORT_BITS, M_SHORT_INT, NO_MOID);
  a68_op (A68_STD, "BIN", m, a68_lower_bin2);
  m = a68_proc (M_SHORT_BITS, M_SHORT_BITS, NO_MOID);
  a68_op (A68_STD, "NOT", m, a68_lower_bitnot2);
  a68_op (A68_STD, "~", m, a68_lower_bitnot2);
  m = a68_proc (M_BOOL, M_SHORT_BITS, M_SHORT_BITS, NO_MOID);
  a68_op (A68_STD, "=", m, a68_lower_bit_eq3);
  a68_op (A68_STD, "EQ", m, a68_lower_bit_eq3);
  a68_op (A68_STD, "/=", m, a68_lower_bit_ne3);
  a68_op (A68_STD, "NE", m, a68_lower_bit_ne3);
  a68_op (A68_STD, "<=", m, a68_lower_bit_le3);
  a68_op (A68_STD, "LE", m, a68_lower_bit_le3);
  a68_op (A68_STD, ">=", m, a68_lower_bit_ge3);
  a68_op (A68_STD, "GE", m, a68_lower_bit_ge3);
  m = a68_proc (M_SHORT_BITS, M_SHORT_BITS, M_SHORT_BITS, NO_MOID);
  a68_op (A68_STD, "AND", m, a68_lower_bitand3);
  a68_op (A68_STD, "OR", m, a68_lower_bitior3);
  m = a68_proc (M_SHORT_BITS, M_SHORT_BITS, M_INT, NO_MOID);
  a68_op (A68_STD, "SHL", m, a68_lower_shl3);
  a68_op (A68_STD, "UP", m, a68_lower_shl3);
  a68_op (A68_STD, "SHR", m, a68_lower_shr3);
  a68_op (A68_STD, "DOWN", m, a68_lower_shr3);
  m = a68_proc (M_BOOL, M_INT, M_SHORT_BITS, NO_MOID);
  a68_op (A68_STD, "ELEM", m, a68_lower_bitelem3);
  m = a68_proc (M_SHORT_SHORT_BITS, M_SHORT_BITS, NO_MOID);
  a68_op (A68_STD, "SHORTEN", m, a68_lower_bitshorten2);
  m = a68_proc (M_BITS, M_SHORT_BITS, NO_MOID);
  a68_op (A68_STD, "LENG", m,  a68_lower_bitleng2);
  /* BITS operators.  */
  m = a68_proc (M_INT, M_BITS, NO_MOID);
  a68_op (A68_STD, "ABS", m, a68_lower_bitabs2);
  m = a68_proc (M_BITS, M_INT, NO_MOID);
  a68_op (A68_STD, "BIN", m, a68_lower_bin2);
  m = a68_proc (M_BITS, M_BITS, NO_MOID);
  a68_op (A68_STD, "NOT", m, a68_lower_bitnot2);
  a68_op (A68_STD, "~", m, a68_lower_bitnot2);
  m = a68_proc (M_BOOL, M_BITS, M_BITS, NO_MOID);
  a68_op (A68_STD, "=", m, a68_lower_bit_eq3);
  a68_op (A68_STD, "/=", m, a68_lower_bit_ne3);
  a68_op (A68_STD, "<=", m, a68_lower_bit_le3);
  a68_op (A68_STD, ">=", m, a68_lower_bit_ge3);
  a68_op (A68_STD, "EQ", m, a68_lower_bit_eq3);
  a68_op (A68_STD, "NE", m, a68_lower_bit_ne3);
  a68_op (A68_STD, "LE", m, a68_lower_bit_le3);
  a68_op (A68_STD, "GE", m, a68_lower_bit_ge3);
  m = a68_proc (M_BITS, M_BITS, M_BITS, NO_MOID);
  a68_op (A68_STD, "AND", m, a68_lower_bitand3);
  a68_op (A68_STD, "OR", m, a68_lower_bitior3);
  m = a68_proc (M_BITS, M_BITS, M_INT, NO_MOID);
  a68_op (A68_STD, "SHL", m, a68_lower_shl3);
  a68_op (A68_STD, "UP", m, a68_lower_shl3);
  a68_op (A68_STD, "SHR", m, a68_lower_shr3);
  a68_op (A68_STD, "DOWN", m, a68_lower_shr3);
  m = a68_proc (M_BOOL, M_INT, M_BITS, NO_MOID);
  a68_op (A68_STD, "ELEM", m, a68_lower_bitelem3);
  m = a68_proc (M_LONG_BITS, M_BITS, NO_MOID);
  a68_op (A68_STD, "LENG", m, a68_lower_bitleng2);
  m = a68_proc (M_SHORT_BITS, M_BITS, NO_MOID);
  a68_op (A68_STD, "SHORTEN", m, a68_lower_bitshorten2);
  /* LONG BITS operatos.  */
  m = a68_proc (M_LONG_INT, M_LONG_BITS, NO_MOID);
  a68_op (A68_STD, "ABS", m, a68_lower_bitabs2);
  m = a68_proc (M_LONG_BITS, M_LONG_INT, NO_MOID);
  a68_op (A68_STD, "BIN", m, a68_lower_bin2);
  m = a68_proc (M_LONG_BITS, M_LONG_BITS, NO_MOID);
  a68_op (A68_STD, "NOT", m, a68_lower_bitnot2);
  a68_op (A68_STD, "~", m, a68_lower_bitnot2);
  m = a68_proc (M_BOOL, M_LONG_BITS, M_LONG_BITS, NO_MOID);
  a68_op (A68_STD, "=", m, a68_lower_bit_eq3);
  a68_op (A68_STD, "EQ", m, a68_lower_bit_eq3);
  a68_op (A68_STD, "/=", m, a68_lower_bit_ne3);
  a68_op (A68_STD, "NE", m, a68_lower_bit_ne3);
  a68_op (A68_STD, "<=", m, a68_lower_bit_le3);
  a68_op (A68_STD, "LE", m, a68_lower_bit_le3);
  a68_op (A68_STD, ">=", m, a68_lower_bit_ge3);
  a68_op (A68_STD, "GE", m, a68_lower_bit_ge3);
  m = a68_proc (M_LONG_BITS, M_LONG_BITS, M_LONG_BITS, NO_MOID);
  a68_op (A68_STD, "AND", m, a68_lower_bitand3);
  a68_op (A68_STD, "OR", m, a68_lower_bitior3);
  m = a68_proc (M_LONG_BITS, M_LONG_BITS, M_INT, NO_MOID);
  a68_op (A68_STD, "SHL", m, a68_lower_shl3);
  a68_op (A68_STD, "UP", m, a68_lower_shl3);
  a68_op (A68_STD, "SHR", m, a68_lower_shr3);
  a68_op (A68_STD, "DOWN", m, a68_lower_shr3);
  m = a68_proc (M_BOOL, M_INT, M_LONG_BITS, NO_MOID);
  a68_op (A68_STD, "ELEM", m, a68_lower_bitelem3);
  m = a68_proc (M_BITS, M_LONG_BITS, NO_MOID);
  a68_op (A68_STD, "SHORTEN", m, a68_lower_bitshorten2);
  m = a68_proc (M_LONG_LONG_BITS, M_LONG_BITS, NO_MOID);
  a68_op (A68_STD, "LENG", m,  a68_lower_bitleng2);
  /* LONG LONG BITS operators  */
  m = a68_proc (M_BOOL, M_LONG_LONG_BITS, M_LONG_LONG_BITS, NO_MOID);
  a68_op (A68_STD, "=", m, a68_lower_bit_eq3);
  a68_op (A68_STD, "EQ", m, a68_lower_bit_eq3);
  a68_op (A68_STD, "/=", m, a68_lower_bit_ne3);
  a68_op (A68_STD, "NE", m, a68_lower_bit_ne3);
  a68_op (A68_STD, "<=", m, a68_lower_bit_le3);
  a68_op (A68_STD, "LE", m, a68_lower_bit_le3);
  a68_op (A68_STD, ">=", m, a68_lower_bit_ge3);
  a68_op (A68_STD, "GE", m, a68_lower_bit_ge3);
  m = a68_proc (M_LONG_LONG_BITS, M_LONG_LONG_BITS, NO_MOID);
  a68_op (A68_STD, "NOT", m, a68_lower_bitnot2);
  a68_op (A68_STD, "~", m, a68_lower_bitnot2);
  m = a68_proc (M_LONG_LONG_BITS, M_LONG_LONG_BITS, M_LONG_LONG_BITS, NO_MOID);
  a68_op (A68_STD, "AND", m, a68_lower_bitand3);
  a68_op (A68_STD, "OR", m, a68_lower_bitior3);
  m = a68_proc (M_LONG_LONG_BITS, M_LONG_LONG_BITS, M_INT, NO_MOID);
  a68_op (A68_STD, "SHL", m, a68_lower_shl3);
  a68_op (A68_STD, "UP", m, a68_lower_shl3);
  a68_op (A68_STD, "SHR", m, a68_lower_shr3);
  a68_op (A68_STD, "DOWN", m, a68_lower_shr3);
  m = a68_proc (M_BOOL, M_INT, M_LONG_LONG_BITS, NO_MOID);
  a68_op (A68_STD, "ELEM", m, a68_lower_bitelem3);
  m = a68_proc (M_LONG_LONG_BITS, M_LONG_LONG_INT, NO_MOID);
  a68_op (A68_STD, "BIN", m, a68_lower_bin2);
  m = a68_proc (M_LONG_LONG_INT, M_LONG_LONG_BITS, NO_MOID);
  a68_op (A68_STD, "ABS", m, a68_lower_bitabs2);
  m = a68_proc (M_LONG_BITS, M_LONG_LONG_BITS, NO_MOID);
  a68_op (A68_STD, "SHORTEN", m, a68_lower_bitshorten2);
  /* REAL operators.  */
  m = A68_MCACHE (proc_real_real);
  a68_op (A68_STD, "+", m, a68_lower_confirm2);
  a68_op (A68_STD, "-", m, a68_lower_negate2);
  a68_op (A68_STD, "ABS", m, a68_lower_realabs2);
  m = a68_proc (M_INT, M_REAL, NO_MOID);
  a68_op (A68_STD, "SIGN", m, a68_lower_realsign2);
  a68_op (A68_STD, "ROUND", m, a68_lower_round2);
  a68_op (A68_STD, "ENTIER", m, a68_lower_entier2);
  m = a68_proc (M_BOOL, M_REAL, M_REAL, NO_MOID);
  a68_op (A68_STD, "=", m, a68_lower_real_eq3);
  a68_op (A68_STD, "/=", m, a68_lower_real_ne3);
  a68_op (A68_STD, "<", m, a68_lower_real_lt3);
  a68_op (A68_STD, "<=", m, a68_lower_real_le3);
  a68_op (A68_STD, ">", m, a68_lower_real_gt3);
  a68_op (A68_STD, ">=", m, a68_lower_real_ge3);
  a68_op (A68_STD, "EQ", m, a68_lower_real_eq3);
  a68_op (A68_STD, "NE", m, a68_lower_real_ne3);
  a68_op (A68_STD, "LT", m, a68_lower_real_lt3);
  a68_op (A68_STD, "LE", m, a68_lower_real_le3);
  a68_op (A68_STD, "GT", m, a68_lower_real_gt3);
  a68_op (A68_STD, "GE", m, a68_lower_real_ge3);
  m = A68_MCACHE (proc_real_real_real);
  a68_op (A68_STD, "+", m, a68_lower_plus_real);
  a68_op (A68_STD, "-", m, a68_lower_minus_real);
  a68_op (A68_STD, "*", m, a68_lower_mult_real);
  a68_op (A68_STD, "/", m, a68_lower_div3);
  a68_op (A68_STD, "**", m, a68_lower_pow_real);
  m = a68_proc (M_REAL, M_REAL, M_INT, NO_MOID);
  a68_op (A68_STD, "**", m, a68_lower_pow_real);
  m = a68_proc (M_REF_REAL, M_REF_REAL, M_REAL, NO_MOID);
  a68_op (A68_STD, "+:=", m, a68_lower_plusab3);
  a68_op (A68_STD, "-:=", m, a68_lower_minusab3);
  a68_op (A68_STD, "*:=", m, a68_lower_multab3);
  a68_op (A68_STD, "/:=", m, a68_lower_divab3);
  a68_op (A68_STD, "PLUSAB", m, a68_lower_plusab3);
  a68_op (A68_STD, "MINUSAB", m, a68_lower_minusab3);
  a68_op (A68_STD, "TIMESAB", m, a68_lower_multab3);
  a68_op (A68_STD, "DIVAB", m, a68_lower_divab3);
  m = a68_proc (M_LONG_REAL, M_REAL, NO_MOID);
  a68_op (A68_STD, "LENG", m, a68_lower_lengreal2);
  /* LONG REAL operators */
  m = a68_proc (M_LONG_LONG_REAL, M_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "LENG", m, a68_lower_lengreal2);
  m = a68_proc (M_REAL, M_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "SHORTEN", m, a68_lower_shortenreal2);
  m = a68_proc (M_LONG_REAL, M_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "+", m, a68_lower_confirm2);
  a68_op (A68_STD, "-", m, a68_lower_negate2);
  a68_op (A68_STD, "ABS", m, a68_lower_realabs2);
  m = a68_proc (M_INT, M_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "SIGN", m, a68_lower_realsign2);
  m = a68_proc (M_LONG_INT, M_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "ENTIER", m, a68_lower_entier2);
  a68_op (A68_STD, "ROUND", m, a68_lower_round2);
  m = a68_proc (M_LONG_REAL, M_LONG_REAL, M_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "+", m, a68_lower_plus_real);
  a68_op (A68_STD, "-", m, a68_lower_minus_real);
  a68_op (A68_STD, "*", m, a68_lower_mult_real);
  a68_op (A68_STD, "/", m, a68_lower_div3);
  a68_op (A68_STD, "**", m, a68_lower_pow_real);
  m = a68_proc (M_REF_LONG_REAL, M_REF_LONG_REAL, M_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "+:=", m, a68_lower_plusab3);
  a68_op (A68_STD, "-:=", m, a68_lower_minusab3);
  a68_op (A68_STD, "*:=", m, a68_lower_multab3);
  a68_op (A68_STD, "/:=", m, a68_lower_divab3);
  a68_op (A68_STD, "PLUSAB", m, a68_lower_plusab3);
  a68_op (A68_STD, "MINUSAB", m, a68_lower_minusab3);
  a68_op (A68_STD, "TIMESAB", m, a68_lower_multab3);
  a68_op (A68_STD, "DIVAB", m, a68_lower_divab3);
  m = a68_proc (M_BOOL, M_LONG_REAL, M_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "=", m, a68_lower_real_eq3);
  a68_op (A68_STD, "EQ", m, a68_lower_real_eq3);
  a68_op (A68_STD, "/=", m, a68_lower_real_ne3);
  a68_op (A68_STD, "NE", m, a68_lower_real_ne3);
  a68_op (A68_STD, "<", m, a68_lower_real_lt3);
  a68_op (A68_STD, "LT", m, a68_lower_real_lt3);
  a68_op (A68_STD, "<=", m, a68_lower_real_le3);
  a68_op (A68_STD, "LE", m, a68_lower_real_le3);
  a68_op (A68_STD, ">", m, a68_lower_real_gt3);
  a68_op (A68_STD, "GT", m, a68_lower_real_gt3);
  a68_op (A68_STD, ">=", m, a68_lower_real_ge3);
  a68_op (A68_STD, "GE", m, a68_lower_real_ge3);
  m = a68_proc (M_LONG_REAL, M_LONG_REAL, M_INT, NO_MOID);
  a68_op (A68_STD, "**", m, a68_lower_pow_real);
  /* LONG LONG REAL operators. */
  m = a68_proc (M_LONG_REAL, M_LONG_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "SHORTEN", m, a68_lower_shortenreal2);
  m = a68_proc (M_LONG_LONG_REAL, M_LONG_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "ABS", m, a68_lower_realabs2);
  a68_op (A68_STD, "+", m, a68_lower_confirm2);
  a68_op (A68_STD, "-", m, a68_lower_negate2);
  m = a68_proc (M_INT, M_LONG_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "SIGN", m, a68_lower_realsign2);
  m = a68_proc (M_LONG_LONG_INT, M_LONG_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "ENTIER", m, a68_lower_entier2);
  a68_op (A68_STD, "ROUND", m, a68_lower_round2);
  m = a68_proc (M_LONG_LONG_REAL, M_LONG_LONG_REAL, M_LONG_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "+", m, a68_lower_plus_real);
  a68_op (A68_STD, "-", m, a68_lower_minus_real);
  a68_op (A68_STD, "*", m, a68_lower_mult_real);
  a68_op (A68_STD, "/", m, a68_lower_div3);
  a68_op (A68_STD, "**", m, a68_lower_pow_real);
  m = a68_proc (M_REF_LONG_LONG_REAL, M_REF_LONG_LONG_REAL, M_LONG_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "+:=", m, a68_lower_plusab3);
  a68_op (A68_STD, "-:=", m, a68_lower_minusab3);
  a68_op (A68_STD, "*:=", m, a68_lower_multab3);
  a68_op (A68_STD, "/:=", m, a68_lower_divab3);
  a68_op (A68_STD, "PLUSAB", m, a68_lower_plusab3);
  a68_op (A68_STD, "MINUSAB", m, a68_lower_minusab3);
  a68_op (A68_STD, "TIMESAB", m, a68_lower_multab3);
  a68_op (A68_STD, "DIVAB", m, a68_lower_divab3);
  m = a68_proc (M_BOOL, M_LONG_LONG_REAL, M_LONG_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "=", m, a68_lower_real_eq3);
  a68_op (A68_STD, "EQ", m, a68_lower_real_eq3);
  a68_op (A68_STD, "/=", m, a68_lower_real_ne3);
  a68_op (A68_STD, "NE", m, a68_lower_real_ne3);
  a68_op (A68_STD, "<", m, a68_lower_real_lt3);
  a68_op (A68_STD, "LT", m, a68_lower_real_lt3);
  a68_op (A68_STD, "<=", m, a68_lower_real_le3);
  a68_op (A68_STD, "LE", m, a68_lower_real_le3);
  a68_op (A68_STD, ">", m, a68_lower_real_gt3);
  a68_op (A68_STD, "GT", m, a68_lower_real_gt3);
  a68_op (A68_STD, ">=", m, a68_lower_real_ge3);
  a68_op (A68_STD, "GE", m, a68_lower_real_ge3);
  m = a68_proc (M_LONG_LONG_REAL, M_LONG_LONG_REAL, M_INT, NO_MOID);
  a68_op (A68_STD, "**", m, a68_lower_pow_real);
  /* ROWS operators.  */
  m = a68_proc (M_INT, M_ROWS, NO_MOID);
  a68_op (A68_STD, "LWB", m, a68_lower_lwb2);
  a68_op (A68_STD, "UPB", m, a68_lower_upb2);
  m = a68_proc (M_INT, M_INT, M_ROWS, NO_MOID);
  a68_op (A68_STD, "LWB", m, a68_lower_lwb3);
  a68_op (A68_STD, "UPB", m, a68_lower_upb3);
  /* BYTES operators.  */
  m = a68_proc (M_BYTES, M_STRING, NO_MOID);
  a68_idf (A68_STD, "bytespack", m);
  m = a68_proc (M_CHAR, M_INT, M_BYTES, NO_MOID);
  a68_op (A68_STD, "ELEM", m);
  m = a68_proc (M_BYTES, M_BYTES, M_BYTES, NO_MOID);
  a68_op (A68_STD, "+", m);
  m = a68_proc (M_REF_BYTES, M_REF_BYTES, M_BYTES, NO_MOID);
  a68_op (A68_STD, "+:=", m);
  a68_op (A68_STD, "PLUSAB", m);
  m = a68_proc (M_BOOL, M_BYTES, M_BYTES, NO_MOID);
  a68_op (A68_STD, "=", m);
  a68_op (A68_STD, "/=", m);
  a68_op (A68_STD, "<", m);
  a68_op (A68_STD, "<=", m);
  a68_op (A68_STD, ">", m);
  a68_op (A68_STD, ">=", m);
  a68_op (A68_STD, "EQ", m);
  a68_op (A68_STD, "NE", m);
  a68_op (A68_STD, "LT", m);
  a68_op (A68_STD, "LE", m);
  a68_op (A68_STD, "GT", m);
  a68_op (A68_STD, "GE", m);
  /* LONG BYTES operators.  */
  m = a68_proc (M_LONG_BYTES, M_BYTES, NO_MOID);
  a68_op (A68_STD, "LENG", m);
  m = a68_proc (M_BYTES, M_LONG_BYTES, NO_MOID);
  a68_idf (A68_STD, "SHORTEN", m);
  m = a68_proc (M_LONG_BYTES, M_STRING, NO_MOID);
  a68_idf (A68_STD, "longbytespack", m);
  m = a68_proc (M_CHAR, M_INT, M_LONG_BYTES, NO_MOID);
  a68_op (A68_STD, "ELEM", m);
  m = a68_proc (M_LONG_BYTES, M_LONG_BYTES, M_LONG_BYTES, NO_MOID);
  a68_op (A68_STD, "+", m);
  m = a68_proc (M_REF_LONG_BYTES, M_REF_LONG_BYTES, M_LONG_BYTES, NO_MOID);
  a68_op (A68_STD, "+:=", m);
  a68_op (A68_STD, "PLUSAB", m);
  m = a68_proc (M_BOOL, M_LONG_BYTES, M_LONG_BYTES, NO_MOID);
  a68_op (A68_STD, "=", m);
  a68_op (A68_STD, "/=", m);
  a68_op (A68_STD, "<", m);
  a68_op (A68_STD, "<=", m);
  a68_op (A68_STD, ">", m);
  a68_op (A68_STD, ">=", m);
  a68_op (A68_STD, "EQ", m);
  a68_op (A68_STD, "NE", m);
  a68_op (A68_STD, "LT", m);
  a68_op (A68_STD, "LE", m);
  a68_op (A68_STD, "GT", m);
  a68_op (A68_STD, "GE", m);
  /* COMPLEX operators.  */
  m = a68_proc (M_COMPLEX, M_REAL, M_REAL, NO_MOID);
  a68_op (A68_STD, "I", m, a68_lower_reali);
  a68_op (A68_STD, "+*", m, a68_lower_reali);
  m = a68_proc (M_COMPLEX, M_INT, M_INT, NO_MOID);
  a68_op (A68_STD, "I", m, a68_lower_inti);
  a68_op (A68_STD, "+*", m, a68_lower_inti);
  m = a68_proc (M_REAL, M_COMPLEX, NO_MOID);
  a68_op (A68_STD, "RE", m, a68_lower_re2);
  a68_op (A68_STD, "IM", m, a68_lower_im2);
  a68_op (A68_STD, "ABS", m);
  a68_op (A68_STD, "ARG", m);
  m = A68_MCACHE (proc_complex_complex);
  a68_op (A68_STD, "+", m);
  a68_op (A68_STD, "-", m);
  a68_op (A68_STD, "CONJ", m, a68_lower_conj2);
  m = a68_proc (M_BOOL, M_COMPLEX, M_COMPLEX, NO_MOID);
  a68_op (A68_STD, "=", m);
  a68_op (A68_STD, "/=", m);
  a68_op (A68_STD, "EQ", m);
  a68_op (A68_STD, "NE", m);
  m = a68_proc (M_COMPLEX, M_COMPLEX, M_COMPLEX, NO_MOID);
  a68_op (A68_STD, "+", m);
  a68_op (A68_STD, "-", m);
  a68_op (A68_STD, "*", m);
  a68_op (A68_STD, "/", m);
  m = a68_proc (M_COMPLEX, M_COMPLEX, M_INT, NO_MOID);
  a68_op (A68_STD, "**", m);
  m = a68_proc (M_REF_COMPLEX, M_REF_COMPLEX, M_COMPLEX, NO_MOID);
  a68_op (A68_STD, "+:=", m);
  a68_op (A68_STD, "-:=", m);
  a68_op (A68_STD, "*:=", m);
  a68_op (A68_STD, "/:=", m);
  a68_op (A68_STD, "PLUSAB", m);
  a68_op (A68_STD, "MINUSAB", m);
  a68_op (A68_STD, "TIMESAB", m);
  a68_op (A68_STD, "DIVAB", m);
  m = a68_proc (M_COMPLEX, M_COMPLEX, NO_MOID);
  a68_op (A68_STD, "SHORTEN", m);
  /* LONG COMPLEX  operators */
  m = a68_proc (M_LONG_COMPLEX, M_LONG_INT, M_LONG_INT, NO_MOID);
  a68_op (A68_STD, "I", m, a68_lower_longinti);
  a68_op (A68_STD, "+*", m, a68_lower_longinti);
  m = a68_proc (M_LONG_COMPLEX, M_LONG_REAL, M_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "I", m, a68_lower_longreali);
  a68_op (A68_STD, "+*", m, a68_lower_longreali);
  m = a68_proc (M_LONG_LONG_COMPLEX, M_LONG_COMPLEX, NO_MOID);
  a68_op (A68_STD, "LENG", m);
  m = a68_proc (M_LONG_COMPLEX, M_LONG_LONG_COMPLEX, NO_MOID);
  a68_op (A68_STD, "SHORTEN", m);
  m = a68_proc (M_LONG_COMPLEX, M_COMPLEX, NO_MOID);
  a68_op (A68_STD, "LENG", m);
  m = a68_proc (M_COMPLEX, M_LONG_COMPLEX, NO_MOID);
  a68_op (A68_STD, "SHORTEN", m);
  m = a68_proc (M_LONG_REAL, M_LONG_COMPLEX, NO_MOID);
  a68_op (A68_STD, "RE", m, a68_lower_re2);
  a68_op (A68_STD, "IM", m, a68_lower_im2);
  a68_op (A68_STD, "ARG", m);
  a68_op (A68_STD, "ABS", m);
  m = a68_proc (M_LONG_COMPLEX, M_LONG_COMPLEX, NO_MOID);
  a68_op (A68_STD, "+", m);
  a68_op (A68_STD, "-", m);
  a68_op (A68_STD, "CONJ", m, a68_lower_conj2);
  m = a68_proc (M_LONG_COMPLEX, M_LONG_COMPLEX, M_LONG_COMPLEX, NO_MOID);
  a68_op (A68_STD, "+", m);
  a68_op (A68_STD, "-", m);
  a68_op (A68_STD, "*", m);
  a68_op (A68_STD, "/", m);
  m = a68_proc (M_LONG_COMPLEX, M_LONG_COMPLEX, M_INT, NO_MOID);
  a68_op (A68_STD, "**", m);
  m = a68_proc (M_BOOL, M_LONG_COMPLEX, M_LONG_COMPLEX, NO_MOID);
  a68_op (A68_STD, "=", m);
  a68_op (A68_STD, "EQ", m);
  a68_op (A68_STD, "/=", m);
  a68_op (A68_STD, "NE", m);
  m = a68_proc (M_REF_LONG_COMPLEX, M_REF_LONG_COMPLEX, M_LONG_COMPLEX, NO_MOID);
  a68_op (A68_STD, "+:=", m);
  a68_op (A68_STD, "-:=", m);
  a68_op (A68_STD, "*:=", m);
  a68_op (A68_STD, "/:=", m);
  a68_op (A68_STD, "PLUSAB", m);
  a68_op (A68_STD, "MINUSAB", m);
  a68_op (A68_STD, "TIMESAB", m);
  a68_op (A68_STD, "DIVAB", m);
  /* LONG LONG COMPLEX operators.  */
  m = a68_proc (M_LONG_LONG_COMPLEX, M_LONG_LONG_INT, M_LONG_LONG_INT, NO_MOID);
  a68_op (A68_STD, "I", m, a68_lower_longlonginti);
  a68_op (A68_STD, "+*", m, a68_lower_longlonginti);
  m = a68_proc (M_LONG_LONG_COMPLEX, M_LONG_LONG_REAL, M_LONG_LONG_REAL, NO_MOID);
  a68_op (A68_STD, "I", m, a68_lower_longlongreali);
  a68_op (A68_STD, "+*", m, a68_lower_longlongreali);
  m = a68_proc (M_LONG_LONG_REAL, M_LONG_LONG_COMPLEX, NO_MOID);
  a68_op (A68_STD, "RE", m, a68_lower_re2);
  a68_op (A68_STD, "IM", m, a68_lower_im2);
  a68_op (A68_STD, "ARG", m);
  a68_op (A68_STD, "ABS", m);
  m = a68_proc (M_LONG_LONG_COMPLEX, M_LONG_LONG_COMPLEX, NO_MOID);
  a68_op (A68_STD, "+", m);
  a68_op (A68_STD, "-", m);
  a68_op (A68_STD, "CONJ", m, a68_lower_conj2);
  m = a68_proc (M_LONG_LONG_COMPLEX, M_LONG_LONG_COMPLEX, M_LONG_LONG_COMPLEX, NO_MOID);
  a68_op (A68_STD, "+", m);
  a68_op (A68_STD, "-", m);
  a68_op (A68_STD, "*", m);
  a68_op (A68_STD, "/", m);
  m = a68_proc (M_LONG_LONG_COMPLEX, M_LONG_LONG_COMPLEX, M_INT, NO_MOID);
  a68_op (A68_STD, "**", m);
  m = a68_proc (M_BOOL, M_LONG_LONG_COMPLEX, M_LONG_LONG_COMPLEX, NO_MOID);
  a68_op (A68_STD, "=", m);
  a68_op (A68_STD, "EQ", m);
  a68_op (A68_STD, "/=", m);
  a68_op (A68_STD, "NE", m);
  m = a68_proc (M_REF_LONG_LONG_COMPLEX, M_REF_LONG_LONG_COMPLEX, M_LONG_LONG_COMPLEX, NO_MOID);
  a68_op (A68_STD, "+:=", m);
  a68_op (A68_STD, "-:=", m);
  a68_op (A68_STD, "*:=", m);
  a68_op (A68_STD, "/:=", m);
  a68_op (A68_STD, "PLUSAB", m);
  a68_op (A68_STD, "MINUSAB", m);
  a68_op (A68_STD, "TIMESAB", m);
  a68_op (A68_STD, "DIVAB", m);
  m = a68_proc (M_LONG_LONG_COMPLEX, M_LONG_LONG_COMPLEX, NO_MOID);
  a68_op (A68_STD, "LENG", m);
  /* SEMA operators.  */
  m = a68_proc (M_SEMA, M_INT, NO_MOID);
  a68_op (A68_STD, "LEVEL", m);
  m = a68_proc (M_INT, M_SEMA, NO_MOID);
  a68_op (A68_STD, "LEVEL", m);
  m = a68_proc (M_VOID, M_SEMA, NO_MOID);
  a68_op (A68_STD, "UP", m);
  a68_op (A68_STD, "DOWN", m);
}

/* GNU extensions for the standenv.  */

static void
gnu_prelude (void)
{
  MOID_T *m = NO_MOID;
  /* Priorities.  */
  a68_prio ("ELEMS", 8);
  /* Identifiers.  */
  a68_idf (A68_EXT, "infinity", M_REAL, a68_lower_infinity);
  a68_idf (A68_EXT, "minusinfinity", M_REAL, a68_lower_minusinfinity);
  a68_idf (A68_EXT, "longlonginfinity", M_LONG_LONG_REAL);
  a68_idf (A68_EXT, "longlongminusinfinity", M_LONG_LONG_REAL);
  a68_idf (A68_EXT, "longinfinity", M_LONG_REAL);
  a68_idf (A68_EXT, "longminusinfinity", M_LONG_REAL);
  a68_idf (A68_EXT, "minint", M_INT, a68_lower_minint);
  a68_idf (A68_EXT, "longminint", M_LONG_INT, a68_lower_minint);
  a68_idf (A68_EXT, "longlongminint", M_LONG_LONG_INT, a68_lower_minint);
  a68_idf (A68_EXT, "shortminint", M_SHORT_INT, a68_lower_minint);
  a68_idf (A68_EXT, "shortshortminint", M_SHORT_SHORT_INT, a68_lower_minint);
  a68_idf (A68_EXT, "minreal", M_REAL, a68_lower_minreal);
  a68_idf (A68_EXT, "longminreal", M_LONG_REAL, a68_lower_minreal);
  a68_idf (A68_EXT, "longlongminreal", M_LONG_LONG_REAL, a68_lower_minreal);
  a68_idf (A68_EXT, "eofchar", M_CHAR, a68_lower_eofchar);
  a68_idf (A68_EXT, "replacementchar", M_CHAR, a68_lower_replacementchar);
  /* REAL procedures.  */
  m = A68_MCACHE (proc_real_real);
  a68_idf (A68_EXT, "log", m, a68_lower_log);
  /* LONG REAL procedures.  */
  m = a68_proc (M_LONG_REAL, M_LONG_REAL, NO_MOID);
  a68_idf (A68_EXT, "longlog", m, a68_lower_long_log);
  /* LONG LONG REAL procedures.  */
  m = a68_proc (M_LONG_LONG_REAL, M_LONG_LONG_REAL, NO_MOID);
  a68_idf (A68_EXT, "longlonglog", m, a68_lower_long_long_log);
  /* BOOL operators.  */
  m = a68_proc (M_BOOL, M_BOOL, M_BOOL, NO_MOID);
  a68_op (A68_EXT, "XOR", m, a68_lower_xor3);
  /* SHORT SHORT BITS operators.  */
  m = a68_proc (M_SHORT_SHORT_BITS, M_SHORT_SHORT_BITS, M_SHORT_SHORT_BITS, NO_MOID);
  a68_op (A68_EXT, "XOR", m, a68_lower_bitxor3);
  /* SHORT BITS operators.  */
  m = a68_proc (M_SHORT_BITS, M_SHORT_BITS, M_SHORT_BITS, NO_MOID);
  a68_op (A68_EXT, "XOR", m, a68_lower_bitxor3);
  /* BITS operators.  */
  m = a68_proc (M_BITS, M_BITS, M_BITS, NO_MOID);
  a68_op (A68_EXT, "XOR", m, a68_lower_bitxor3);
  /* LONG BITS operators.  */
  m = a68_proc (M_LONG_BITS, M_LONG_BITS, M_LONG_BITS, NO_MOID);
  a68_op (A68_EXT, "XOR", m, a68_lower_bitxor3);
  /* LONG LONG BITS operators.  */
  m = a68_proc (M_LONG_LONG_BITS, M_LONG_LONG_BITS, M_LONG_LONG_BITS, NO_MOID);
  a68_op (A68_EXT, "XOR", m, a68_lower_bitxor3);
  /* ROWS operators.  */
  m = a68_proc (M_INT, M_ROWS, NO_MOID);
  a68_op (A68_EXT, "ELEMS", m, a68_lower_elems2);
  m = a68_proc (M_INT, M_INT, M_ROWS, NO_MOID);
  a68_op (A68_EXT, "ELEMS", m, a68_lower_elems3);
}

/* POSIX prelude.  */

static void
posix_prelude (void)
{
  MOID_T *m = NO_MOID;

  /* Environment variables.  */
  m = a68_proc (M_STRING, M_STRING, NO_MOID);
  a68_idf (A68_EXT, "getenv", m, a68_lower_posixgetenv);
  /* Exit status handling.  */
  m = a68_proc (M_VOID, M_INT, NO_MOID);
  a68_idf (A68_EXT, "posixexit", m, a68_lower_posixexit);
  /* Argument handling.  */
  m = A68_MCACHE (proc_int);
  a68_idf (A68_EXT, "argc", m, a68_lower_posixargc);
  m = a68_proc (M_STRING, M_INT, NO_MOID);
  a68_idf (A68_EXT, "argv", m, a68_lower_posixargv);
  /* Error procedures.  */
  m = A68_MCACHE (proc_int);
  a68_idf (A68_EXT, "errno", m, a68_lower_posixerrno);
  m = a68_proc (M_VOID, M_STRING, NO_MOID);
  a68_idf (A68_EXT, "perror", m, a68_lower_posixperror);
  m = a68_proc (M_STRING, M_INT, NO_MOID);
  a68_idf (A68_EXT, "strerror", m, a68_lower_posixstrerror);
  /* I/O identifiers.  */
  a68_idf (A68_EXT, "stdin", M_INT, a68_lower_posixstdinfiledes);
  a68_idf (A68_EXT, "stdout", M_INT, a68_lower_posixstdoutfiledes);
  a68_idf (A68_EXT, "stderr", M_INT, a68_lower_posixstderrfiledes);
  a68_idf (A68_EXT, "fileodefault", M_BITS, a68_lower_posixfileodefault);
  a68_idf (A68_EXT, "fileordwr", M_BITS, a68_lower_posixfileordwr);
  a68_idf (A68_EXT, "fileordonly", M_BITS, a68_lower_posixfileordonly);
  a68_idf (A68_EXT, "fileowronly", M_BITS, a68_lower_posixfileowronly);
  a68_idf (A68_EXT, "fileotrunc", M_BITS, a68_lower_posixfileotrunc);
  /* Opening and closing files.  */
  m = a68_proc (M_INT, M_STRING, M_BITS, NO_MOID);
  a68_idf (A68_EXT, "fopen", m, a68_lower_posixfopen);
  a68_idf (A68_EXT, "fcreate", m, a68_lower_posixfcreate);
  m = A68_MCACHE (proc_int_int);
  a68_idf (A68_EXT, "fclose", m, a68_lower_posixfclose);
  /* Getting properties of files.  */
  m = a68_proc (M_LONG_LONG_INT, M_INT, NO_MOID);
  a68_idf (A68_EXT, "fsize", m, a68_lower_posixfsize);
  m = a68_proc (M_LONG_LONG_INT, M_INT, M_LONG_LONG_INT, M_INT, NO_MOID);
  a68_idf (A68_EXT, "lseek", m, a68_lower_posixlseek);
  a68_idf (A68_EXT, "seekcur", M_INT, a68_lower_posixseekcur);
  a68_idf (A68_EXT, "seekend", M_INT, a68_lower_posixseekend);
  a68_idf (A68_EXT, "seekset", M_INT, a68_lower_posixseekset);
  /* Sockets.  */
  m = a68_proc (M_INT, M_STRING, M_INT, NO_MOID);
  a68_idf (A68_EXT, "fconnect", m, a68_lower_posixfconnect);
  /* String and character output.  */
  m = a68_proc (M_CHAR, M_CHAR, NO_MOID);
  a68_idf (A68_EXT, "putchar", m, a68_lower_posixputchar);
  m = a68_proc (M_VOID, M_STRING, NO_MOID);
  a68_idf (A68_EXT, "puts", m, a68_lower_posixputs);
  m = a68_proc (M_CHAR, M_INT, M_CHAR, NO_MOID);
  a68_idf (A68_EXT, "fputc", m, a68_lower_posixfputc);
  m = a68_proc (M_INT, M_INT, M_STRING, NO_MOID);
  a68_idf (A68_EXT, "fputs", m, a68_lower_posixfputs);
  /* String and character input.  */
  m = A68_MCACHE (proc_char);
  a68_idf (A68_EXT, "getchar", m, a68_lower_posixgetchar);
  m = a68_proc (M_CHAR, M_INT, NO_MOID);
  a68_idf (A68_EXT, "fgetc", m, a68_lower_posixfgetc);
  m = a68_proc (M_REF_STRING, M_INT, NO_MOID);
  a68_idf (A68_EXT, "gets", m, a68_lower_posixgets);
  m = a68_proc (M_REF_STRING, M_INT, M_INT, NO_MOID);
  a68_idf (A68_EXT, "fgets", m, a68_lower_posixfgets);
}

/* Transput.  */

static void
stand_transput (void)
{
  /* Most of the standard transput is implemented in Algol 68 and doesn't
     require compiler support.  See libga68/transput.a68.in */
}

/* Build the standard environ symbol table.  */

void
a68_make_standard_environ (void)
{
  stand_moids ();
  A68_MCACHE (proc_bool) = a68_proc (M_BOOL, NO_MOID);
  A68_MCACHE (proc_char) = a68_proc (M_CHAR, NO_MOID);
  A68_MCACHE (proc_complex_complex) = a68_proc (M_COMPLEX, M_COMPLEX, NO_MOID);
  A68_MCACHE (proc_int) = a68_proc (M_INT, NO_MOID);
  A68_MCACHE (proc_int_int) = a68_proc (M_INT, M_INT, NO_MOID);
  A68_MCACHE (proc_int_int_real) = a68_proc (M_REAL, M_INT, M_INT, NO_MOID);
  A68_MCACHE (proc_int_real) = a68_proc (M_REAL, M_INT, NO_MOID);
  A68_MCACHE (proc_int_real_real) = a68_proc (M_REAL, M_INT, M_REAL, NO_MOID);
  A68_MCACHE (proc_int_real_real_real) = a68_proc (M_REAL, M_INT, M_REAL, M_REAL, NO_MOID);
  A68_MCACHE (proc_real) = a68_proc (M_REAL, NO_MOID);
  A68_MCACHE (proc_real_int_real) = a68_proc (M_REAL, M_REAL, M_INT, NO_MOID);
  A68_MCACHE (proc_real_real_int_real) = a68_proc (M_REAL, M_REAL, M_REAL, M_INT, NO_MOID);
  A68_MCACHE (proc_real_real) = M_PROC_REAL_REAL;
  A68_MCACHE (proc_real_real_real) = a68_proc (M_REAL, M_REAL, M_REAL, NO_MOID);
  A68_MCACHE (proc_real_real_real_int) = a68_proc (M_INT, M_REAL, M_REAL, M_REAL, NO_MOID);
  A68_MCACHE (proc_real_real_real_real) = a68_proc (M_REAL, M_REAL, M_REAL, M_REAL, NO_MOID);
  A68_MCACHE (proc_real_real_real_real_real) = a68_proc (M_REAL, M_REAL, M_REAL, M_REAL, M_REAL, NO_MOID);
  A68_MCACHE (proc_real_real_real_real_real_real) = a68_proc (M_REAL, M_REAL, M_REAL, M_REAL, M_REAL, M_REAL, NO_MOID);
  A68_MCACHE (proc_real_ref_real_ref_int_void) = a68_proc (M_VOID, M_REAL, M_REF_REAL, M_REF_INT, NO_MOID);
  A68_MCACHE (proc_void) = a68_proc (M_VOID, NO_MOID);
  stand_prelude ();
  if (!OPTION_STRICT (&A68_JOB))
    {
      gnu_prelude ();
      posix_prelude ();
    }
  stand_transput ();
}
