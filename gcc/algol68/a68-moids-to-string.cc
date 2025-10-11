/* Pretty-print a MOID.
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

/*
 * A pretty printer for moids.
 *
 * For example "PROC (REF STRUCT (REF SELF, UNION (INT, VOID))) REF SELF"
 * for a procedure yielding a pointer to an object of its own mode.
 */

static void moid_to_string_2 (char *, MOID_T *, size_t *, NODE_T *,
			      bool indicant_value);

/* Add string to MOID text.  */

static void
add_to_moid_text (char *dst, const char *str, size_t *w)
{
  a68_bufcat (dst, str, BUFFER_SIZE);
  (*w) -= strlen (str);
}

/* Find a tag, searching symbol tables towards the root.  */

static TAG_T *
find_indicant_global (TABLE_T * table, MOID_T * mode)
{
  if (table != NO_TABLE)
    {
      for (TAG_T *s = INDICANTS (table); s != NO_TAG; FORWARD (s))
	{
	  if (MOID (s) == mode)
	    return s;
	}
      return find_indicant_global (PREVIOUS (table), mode);
    }
  else
    return NO_TAG;
}

/* Pack to string.  */

static void
pack_to_string (char *b, PACK_T *p, size_t *w, bool text, NODE_T *idf,
		bool indicant_value)
{
  for (; p != NO_PACK; FORWARD (p))
    {
      moid_to_string_2 (b, MOID (p), w, idf, indicant_value);
      if (text)
	{
	  if (TEXT (p) != NO_TEXT)
	    {
	      add_to_moid_text (b, " ", w);
	      add_to_moid_text (b, TEXT (p), w);
	    }
	}
      if (p != NO_PACK && NEXT (p) != NO_PACK)
	add_to_moid_text (b, ", ", w);
    }
}

/* Moid to string 2.  */

static void moid_to_string_2 (char *b, MOID_T *n, size_t *w, NODE_T *idf,
			      bool indicant_value)
{
  bool supper_stropping = (OPTION_STROPPING (&A68_JOB) == SUPPER_STROPPING);
  const char *strop_self = supper_stropping ? "self" : "SELF";
  const char *strop_hip = supper_stropping ? "hip" : "HIP";
  const char *strop_compl = supper_stropping ? "compl" : "COMPL";
  const char *strop_long_compl = supper_stropping ? "long compl" : "LONG COMPL";
  const char *strop_long_long_compl = supper_stropping ? "long long compl" : "LONG LONG COMPL";
  const char *strop_string = supper_stropping ? "string" : "STRING";
  const char *strop_collitem = supper_stropping ? "collitem" : "COLLITEM";
  const char *strop_simplin = supper_stropping ? "%%<simplin%%>" : "%%<SIMPLIN%%>";
  const char *strop_simplout = supper_stropping ? "%%<simplout%%>" : "%%<SIMPLOUT%%>";
  const char *strop_rows = supper_stropping ? "%%<rows%%>" : "%%<ROWS%%>";
  const char *strop_vacuum = supper_stropping ? "%%<vacuum%%>" : "%%<VACUUM%%>";
  const char *strop_long = supper_stropping ? "long" : "LONG";
  const char *strop_short = supper_stropping ? "short" : "SHORT";
  const char *strop_ref = supper_stropping ? "ref" : "REF";
  const char *strop_flex = supper_stropping ? "flex" : "FLEX";
  const char *strop_struct = supper_stropping ? "struct" : "STRUCT";
  const char *strop_union = supper_stropping ? "union" : "UNION";
  const char *strop_proc = supper_stropping ? "proc" : "PROC";

  if (n == NO_MOID)
    {
      /* Oops. Should not happen.  */
      add_to_moid_text (b, "null", w);;
      return;
    }

  /* Reference to self through REF or PROC.  */
  if (a68_is_postulated (A68 (postulates), n))
    {
      add_to_moid_text (b, strop_self, w);
      return;
    }

  /* If declared by a mode-declaration, present the indicant.  */
  if (idf != NO_NODE && !IS (n, STANDARD))
    {
      TAG_T *indy = find_indicant_global (TABLE (idf), n);

      if (indy != NO_TAG)
	{
	  add_to_moid_text (b, NSYMBOL (NODE (indy)), w);
	  if (!indicant_value)
	    return;
	  else
	    add_to_moid_text (b, " = ", w);
	}
    }

  /* Write the standard modes.  */
  if (n == M_HIP)
    add_to_moid_text (b, strop_hip, w);
  else if (n == M_ERROR)
    add_to_moid_text (b, "ERROR", w);
  else if (n == M_UNDEFINED)
    add_to_moid_text (b, "unresolved mode", w);
  else if (n == M_C_STRING)
    add_to_moid_text (b, "C-STRING", w);
  else if (n == M_COMPLEX)
    add_to_moid_text (b, strop_compl, w);
  else if (n == M_LONG_COMPLEX)
    add_to_moid_text (b, strop_long_compl, w);
  else if (n == M_LONG_LONG_COMPLEX)
    add_to_moid_text (b, strop_long_long_compl, w);
  else if (n == M_STRING)
    add_to_moid_text (b, strop_string, w);
  else if (n == M_COLLITEM)
    add_to_moid_text (b, strop_collitem, w);
  else if (IS (n, IN_TYPE_MODE))
    add_to_moid_text (b, strop_simplin, w);
  else if (IS (n, OUT_TYPE_MODE))
    add_to_moid_text (b, strop_simplout, w);
  else if (IS (n, ROWS_SYMBOL))
    add_to_moid_text (b, strop_rows, w);
  else if (n == M_VACUUM)
    add_to_moid_text (b, strop_vacuum, w);
  else if (IS (n, VOID_SYMBOL) || IS (n, STANDARD) || IS (n, INDICANT))
    {
      if (DIM (n) > 0)
	{
	  size_t k = DIM (n);

	  if ((*w) >= k * strlen ("LONG ") + strlen (NSYMBOL (NODE (n))))
	    {
	      while (k--)
		{
		  add_to_moid_text (b, strop_long, w);
		  add_to_moid_text (b, " ", w);
		}

	      const char *strop_symbol = a68_strop_keyword (NSYMBOL (NODE (n)));
	      add_to_moid_text (b, strop_symbol, w);
	    }
	  else
	    add_to_moid_text (b, "..", w);
	}
      else if (DIM (n) < 0)
	{
	  size_t k = -DIM (n);

	  if ((*w) >= k * strlen ("SHORT ") + strlen (NSYMBOL (NODE (n))))
	    {
	      while (k--)
		{
		  add_to_moid_text (b, strop_short, w);
		  add_to_moid_text (b, " ", w);
		}

	      const char *strop_symbol = a68_strop_keyword (NSYMBOL (NODE (n)));
	      add_to_moid_text (b, strop_symbol, w);
	    }
	  else
	    add_to_moid_text (b, "..", w);
	}
      else if (DIM (n) == 0)
	{
	  const char *strop_symbol = a68_strop_keyword (NSYMBOL (NODE (n)));
	  add_to_moid_text (b, strop_symbol, w);
	}

      /*  Write compxounded modes.  */
    }
  else if (IS_REF (n))
    {
      if ((*w) >= strlen ("REF .."))
	{
	  add_to_moid_text (b, strop_ref, w);
	  add_to_moid_text (b, " ", w);
	  moid_to_string_2 (b, SUB (n), w, idf, indicant_value);
	}
      else
	{
	  add_to_moid_text (b, strop_ref, w);
	  add_to_moid_text (b, " ..", w);
	}
    }
  else if (IS_FLEX (n))
    {
      if ((*w) >= strlen ("FLEX .."))
	{
	  add_to_moid_text (b, strop_flex, w);
	  add_to_moid_text (b, " ", w);
	  moid_to_string_2 (b, SUB (n), w, idf, indicant_value);
	}
      else
	{
	  add_to_moid_text (b, strop_flex, w);
	  add_to_moid_text (b, " ..", w);
	}
    }
  else if (IS_ROW (n))
    {
      size_t j = strlen ("[] ..") + (DIM (n) - 1) * strlen (",");

      if ((*w) >= j)
	{
	  size_t k = DIM (n) - 1;
	  add_to_moid_text (b, "[", w);
	  while (k-- > 0)
	    add_to_moid_text (b, ",", w);
	  add_to_moid_text (b, "] ", w);
	  moid_to_string_2 (b, SUB (n), w, idf, indicant_value);
	}
      else if (DIM (n) == 1)
	{
	  add_to_moid_text (b, "[] ..", w);
	}
      else
	{
	  size_t k = DIM (n);
	  add_to_moid_text (b, "[", w);
	  while (k--)
	    add_to_moid_text (b, ",", w);
	  add_to_moid_text (b, "] ..", w);
	}
    }
  else if (IS_STRUCT (n))
    {
      size_t j = (strlen ("STRUCT ()") + (DIM (n) - 1)
		  * strlen (".., ") + strlen (".."));

      if ((*w) >= j)
	{
	  POSTULATE_T *save = A68 (postulates);
	  a68_make_postulate (&A68 (postulates), n, NO_MOID);
	  add_to_moid_text (b, strop_struct, w);
	  add_to_moid_text (b, " (", w);
	  pack_to_string (b, PACK (n), w, true, idf, indicant_value);
	  add_to_moid_text (b, ")", w);
	  a68_free_postulate_list (A68 (postulates), save);
	  A68 (postulates) = save;
	}
      else
	{
	  size_t k = DIM (n);
	  add_to_moid_text (b, strop_struct, w);
	  add_to_moid_text (b, " (", w);
	  while (k-- > 0)
	    add_to_moid_text (b, ",", w);
	  add_to_moid_text (b, ")", w);
	}
    }
  else if (IS_UNION (n))
    {
      size_t j = (strlen ("UNION ()") + (DIM (n) - 1)
		  * strlen (".., ") + strlen (".."));

      if ((*w) >= j)
	{
	  POSTULATE_T *save = A68 (postulates);
	  a68_make_postulate (&A68 (postulates), n, NO_MOID);
	  add_to_moid_text (b, strop_union, w);
	  add_to_moid_text (b, " (", w);
	  pack_to_string (b, PACK (n), w, false, idf, indicant_value);
	  add_to_moid_text (b, ")", w);
	  a68_free_postulate_list (A68 (postulates), save);
	  A68 (postulates) = save;
	}
    else
      {
	size_t k = DIM (n);
	add_to_moid_text (b, strop_union, w);
	add_to_moid_text (b, " (", w);
	while (k-- > 0)
	  add_to_moid_text (b, ",", w);
	add_to_moid_text (b, ")", w);
      }
    }
  else if (IS (n, PROC_SYMBOL) && DIM (n) == 0)
    {
      if ((*w) >= strlen ("PROC .."))
	{
	  add_to_moid_text (b, strop_proc, w);
	  add_to_moid_text (b, " ", w);
	  moid_to_string_2 (b, SUB (n), w, idf, indicant_value);
	}
      else
	{
	  add_to_moid_text (b, strop_proc, w);
	  add_to_moid_text (b, " ..", w);
	}
    }
  else if (IS (n, PROC_SYMBOL) && DIM (n) > 0)
    {
      size_t j = (strlen ("PROC () ..") + (DIM (n) - 1)
		  * strlen (".., ") + strlen (".."));

      if ((*w) >= j)
	{
	  POSTULATE_T *save = A68 (postulates);
	  a68_make_postulate (&A68 (postulates), n, NO_MOID);
	  add_to_moid_text (b, strop_proc, w);
	  add_to_moid_text (b, " (", w);
	  pack_to_string (b, PACK (n), w, false, idf, indicant_value);
	  add_to_moid_text (b, ") ", w);
	  moid_to_string_2 (b, SUB (n), w, idf, indicant_value);
	  a68_free_postulate_list (A68 (postulates), save);
	  A68 (postulates) = save;
	}
      else
	{
	  size_t k = DIM (n);

	  add_to_moid_text (b, strop_proc, w);
	  add_to_moid_text (b, " (", w);
	  while (k-- > 0)
	    add_to_moid_text (b, ",", w);
	  add_to_moid_text (b, ") ..", w);
	}
    }
  else if (IS (n, SERIES_MODE) || IS (n, STOWED_MODE))
    {
      size_t j = (strlen ("()") + (DIM (n) - 1)
		  * strlen (".., ") + strlen (".."));

      if ((*w) >= j)
	{
	  add_to_moid_text (b, "(", w);
	  pack_to_string (b, PACK (n), w, false, idf, indicant_value);
	  add_to_moid_text (b, ")", w);
	}
      else
	{
	  size_t k = DIM (n);

	  add_to_moid_text (b, "(", w);
	  while (k-- > 0)
	    add_to_moid_text (b, ",", w);
	  add_to_moid_text (b, ")", w);
	}
    }
  else
    {
      char str[SMALL_BUFFER_SIZE];
      if (snprintf (str, (size_t) SMALL_BUFFER_SIZE, "\\%d", ATTRIBUTE (n)) < 0)
	gcc_unreachable ();
      add_to_moid_text (b, str, w);
    }
}

/* Pretty-formatted mode N; W is a measure of width.  */

const char *
a68_moid_to_string (MOID_T *n, size_t w, NODE_T *idf, bool indicant_value)
{
#define MAX_MTS 8
  /* We use a static buffer of MAX_MTS strings. This value 8 should be safe.
     No more than MAX_MTS calls can be pending in for instance printf.  Instead
     we could allocate each string on the heap but that leaks memory.  */
  static int mts_buff_ptr = 0;
  static char mts_buff[8][BUFFER_SIZE];
  char *a = &(mts_buff[mts_buff_ptr][0]);
  mts_buff_ptr++;
  if (mts_buff_ptr >= MAX_MTS)
    mts_buff_ptr = 0;
  a[0] = '\0';
  if (w >= BUFFER_SIZE)
    w = BUFFER_SIZE - 1;
  A68 (postulates) = NO_POSTULATE;
  if (n != NO_MOID)
    moid_to_string_2 (a, n, &w, idf, indicant_value);
  else
    a68_bufcat (a, "null", BUFFER_SIZE);
  return a;
#undef MAX_MTS
}
