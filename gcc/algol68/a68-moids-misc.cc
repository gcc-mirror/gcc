/* Miscellaneous MOID routines.
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
#include "options.h"

#include "a68.h"

/*
 * MODE checker routines.
 */

/* Absorb nested series modes recursively.  */

void
a68_absorb_series_pack (MOID_T **p)
{
  bool siga;

  do
    {
      PACK_T *z = NO_PACK;

      siga = false;
      for (PACK_T *t = PACK (*p); t != NO_PACK; FORWARD (t))
	{
	  if (MOID (t) != NO_MOID && IS (MOID (t), SERIES_MODE))
	    {
	      siga = true;
	      for (PACK_T *s = PACK (MOID (t)); s != NO_PACK; FORWARD (s))
		a68_add_mode_to_pack (&z, MOID (s), NO_TEXT, NODE (s));
	    }
	  else
	    a68_add_mode_to_pack (&z, MOID (t), NO_TEXT, NODE (t));
	}
      PACK (*p) = z;
    }
  while (siga);
}

/* Make SERIES (u, v).  */

MOID_T *
a68_make_series_from_moids (MOID_T *u, MOID_T *v)
{
  MOID_T *x = a68_new_moid ();

  ATTRIBUTE (x) = SERIES_MODE;
  a68_add_mode_to_pack (&(PACK (x)), u, NO_TEXT, NODE (u));
  a68_add_mode_to_pack (&(PACK (x)), v, NO_TEXT, NODE (v));
  a68_absorb_series_pack (&x);
  DIM (x) = a68_count_pack_members (PACK (x));
  (void) a68_register_extra_mode (&TOP_MOID (&A68_JOB), x);
  if (DIM (x) == 1)
    return MOID (PACK (x));
  else
    return x;
}

/* Absorb firmly related unions in mode.

   For instance invalid UNION (PROC REF UNION (A, B), A, B) -> valid
   UNION (A, B), which is used in balancing conformity clauses.  */

MOID_T *
a68_absorb_related_subsets (MOID_T * m)
{
  /* For instance invalid UNION (PROC REF UNION (A, B), A, B) -> valid UNION
     (A, B), which is used in balancing conformity clauses.  */
  bool siga;

  do
    {
      PACK_T *u = NO_PACK;

      siga = false;
      for (PACK_T *v = PACK (m); v != NO_PACK; FORWARD (v))
	{
	  MOID_T *n = a68_depref_completely (MOID (v));

	  if (IS (n, UNION_SYMBOL) && a68_is_subset (n, m, SAFE_DEFLEXING))
	    {
	      /*  Unpack it.  */
	      for (PACK_T *w = PACK (n); w != NO_PACK; FORWARD (w))
		a68_add_mode_to_pack (&u, MOID (w), NO_TEXT, NODE (w));
	      siga = true;
	    }
	  else
	    a68_add_mode_to_pack (&u, MOID (v), NO_TEXT, NODE (v));
	}
      PACK (m) = a68_absorb_union_pack (u);
    }
  while (siga);
  return m;
}

/* Absorb nested series and united modes recursively.  */

void
a68_absorb_series_union_pack (MOID_T **p)
{
  bool siga;

  do
    {
      PACK_T *z = NO_PACK;

      siga = false;
      for (PACK_T *t = PACK (*p); t != NO_PACK; FORWARD (t))
	{
	  if (MOID (t) != NO_MOID && (IS (MOID (t), SERIES_MODE) || IS (MOID (t), UNION_SYMBOL)))
	    {
	      siga = true;
	      for (PACK_T *s = PACK (MOID (t)); s != NO_PACK; FORWARD (s))
		a68_add_mode_to_pack (&z, MOID (s), NO_TEXT, NODE (s));
	    }
	  else
	    a68_add_mode_to_pack (&z, MOID (t), NO_TEXT, NODE (t));
	}
      PACK (*p) = z;
    }
  while (siga);
}

/* Make united mode, from mode that is a SERIES (..).  */

MOID_T *
a68_make_united_mode (MOID_T *m)
{
  if (m == NO_MOID)
    return M_ERROR;
  else if (ATTRIBUTE (m) != SERIES_MODE)
    return m;

  /* Do not unite a single UNION.  */
  if (DIM (m) == 1 && IS (MOID (PACK (m)), UNION_SYMBOL))
    return MOID (PACK (m));

  /* Straighten the series.  */
  a68_absorb_series_union_pack (&m);
  /* Copy the series into a UNION.  */
  MOID_T *u = a68_new_moid ();
  ATTRIBUTE (u) = UNION_SYMBOL;
  PACK (u) = NO_PACK;
  for (PACK_T *w = PACK (m); w != NO_PACK; FORWARD (w))
    a68_add_mode_to_pack (&(PACK (u)), MOID (w), NO_TEXT, NODE (m));

  /* Absorb and contract the new UNION.  */
  a68_absorb_series_union_pack (&u);
  DIM (u) = a68_count_pack_members (PACK (u));
  PACK (u) = a68_absorb_union_pack (PACK (u));
  a68_contract_union (u);
  DIM (u) = a68_count_pack_members (PACK (u));
  /* A UNION of one mode is that mode itself.  */
  if (DIM (u) == 1)
    return MOID (PACK (u));
  else
    return a68_register_extra_mode (&TOP_MOID (&A68_JOB), u);
}

/* Make SOID data structure.  */

void
a68_make_soid (SOID_T *s, int sort, MOID_T *type, int attribute)
{
  ATTRIBUTE (s) = attribute;
  SORT (s) = sort;
  MOID (s) = type;
  CAST (s) = false;
}

/* Whether mode is not well defined.  */

bool
a68_is_mode_isnt_well (MOID_T *p)
{
  if (p == NO_MOID)
    return true;
  else if (!A68_IF_MODE_IS_WELL (p))
    return true;
  else if (PACK (p) != NO_PACK)
    {
      for (PACK_T *q = PACK (p); q != NO_PACK; FORWARD (q))
	{
	  if (!A68_IF_MODE_IS_WELL (MOID (q)))
	    return true;
	}
    }
  return false;
}

/* Add SOID data to free chain.  */

void
a68_free_soid_list (SOID_T *root)
{
  if (root != NO_SOID)
    {
      SOID_T *q = root;

      for (; NEXT (q) != NO_SOID; FORWARD (q))
	;
      NEXT (q) = A68 (top_soid_list);
      A68 (top_soid_list) = root;
    }
}

/* Add SOID data structure to soid list.  */

void
a68_add_to_soid_list (SOID_T **root, NODE_T *where, SOID_T *soid)
{
  if (*root != NO_SOID)
    a68_add_to_soid_list (&(NEXT (*root)), where, soid);
  else
    {
      SOID_T *new_one;

      if (A68 (top_soid_list) == NO_SOID)
	new_one = (SOID_T *) ggc_cleared_alloc<SOID_T> ();
      else
	{
	  new_one = A68 (top_soid_list);
	  FORWARD (A68 (top_soid_list));
	}

      a68_make_soid (new_one, SORT (soid), MOID (soid), 0);
      NODE (new_one) = where;
      NEXT (new_one) = NO_SOID;
      *root = new_one;
    }
}

/* Pack soids in moid, gather resulting moids from terminators in a clause.  */

MOID_T *
a68_pack_soids_in_moid (SOID_T *top_sl, int attribute)
{
  MOID_T *x = a68_new_moid ();
  PACK_T *t, **p;

  ATTRIBUTE (x) = attribute;
  DIM (x) = 0;
  SUB (x) = NO_MOID;
  EQUIVALENT (x) = NO_MOID;
  SLICE (x) = NO_MOID;
  DEFLEXED (x) = NO_MOID;
  NAME (x) = NO_MOID;
  NEXT (x) = NO_MOID;
  PACK (x) = NO_PACK;
  p = &(PACK (x));
  for (; top_sl != NO_SOID; FORWARD (top_sl))
    {
      t = a68_new_pack ();
      MOID (t) = MOID (top_sl);
      TEXT (t) = NO_TEXT;
      NODE (t) = NODE (top_sl);
      NEXT (t) = NO_PACK;
      DIM (x)++;
      *p = t;
      p = &NEXT (t);
    }

  return a68_register_extra_mode (&TOP_MOID (&A68_JOB), x);
}

/* Whether P is compatible with Q.  */

bool
a68_is_equal_modes (MOID_T *p, MOID_T *q, int deflex)
{
  if (deflex == FORCE_DEFLEXING)
    return DEFLEX (p) == DEFLEX (q);
  else if (deflex == ALIAS_DEFLEXING)
    {
      if (IS (p, REF_SYMBOL) && IS (q, REF_SYMBOL))
	return (p == q
		|| a68_prove_moid_equivalence (p, q)
		|| a68_prove_moid_equivalence (DEFLEX (p), q)
		|| DEFLEX (p) == q);
      else if (!IS (p, REF_SYMBOL) && !IS (q, REF_SYMBOL))
	return (DEFLEX (p) == DEFLEX (q)
		|| a68_prove_moid_equivalence (DEFLEX (p), DEFLEX (q)));
  }
  else if (deflex == SAFE_DEFLEXING)
    {
      if (!IS (p, REF_SYMBOL) && !IS (q, REF_SYMBOL))
	return (DEFLEX (p) == DEFLEX (q)
		|| a68_prove_moid_equivalence (DEFLEX (p), DEFLEX (q)));
    }

  return (p == q || a68_prove_moid_equivalence (p, q));
}

/* Whether mode is deprefable, i.e. whether it can be either deferred or
   deprocedured.  */

bool
a68_is_deprefable (MOID_T *p)
{
  if (IS_REF (p))
    return true;
  else
    return (IS (p, PROC_SYMBOL) && PACK (p) == NO_PACK);
}

/* Deref or deproc the mode P once.  */

MOID_T *
a68_depref_once (MOID_T *p)
{
  if (IS_REF_FLEX (p))
    return SUB_SUB (p);
  else if (IS_REF (p))
    return SUB (p);
  else if (IS (p, PROC_SYMBOL) && PACK (p) == NO_PACK)
    return SUB (p);
  else
    return NO_MOID;
}

/* Depref mode completely.  */

MOID_T *
a68_depref_completely (MOID_T *p)
{
  while (a68_is_deprefable (p))
    p = a68_depref_once (p);
  return p;
}

/* Deproc_completely.  */

MOID_T *
a68_deproc_completely (MOID_T *p)
{
  while (IS (p, PROC_SYMBOL) && PACK (p) == NO_PACK)
    p = a68_depref_once (p);
  return p;
}

/* Depref rows.  */

MOID_T *
a68_depref_rows (MOID_T *p, MOID_T *q)
{
  if (q == M_ROWS)
    {
      while (a68_is_deprefable (p))
	p = a68_depref_once (p);
      return p;
    }
  else
    return q;
}

/* Derow mode, strip FLEX and BOUNDS.  */

MOID_T *
a68_derow (MOID_T *p)
{
  if (IS_ROW (p) || IS_FLEX (p))
    return a68_derow (SUB (p));
  else
    return p;
}

/* Whether rows type.  */

bool
a68_is_rows_type (MOID_T *p)
{
  switch (ATTRIBUTE (p))
    {
    case ROW_SYMBOL:
    case FLEX_SYMBOL:
      return true;
    case UNION_SYMBOL:
      {
	PACK_T *t = PACK (p);
	bool siga = true;
	while (t != NO_PACK && siga)
	  {
	    siga &= a68_is_rows_type (MOID (t));
	    FORWARD (t);
	  }
	return siga;
      }
    default:
      return false;
    }
}

/* Whether mode is PROC (REF FILE) VOID or FORMAT.  */

bool
a68_is_proc_ref_file_void_or_format (MOID_T *p)
{
  if (p == M_PROC_REF_FILE_VOID)
    return true;
  else if (p == M_FORMAT)
    return true;
  else
    return false;
}

/* Whether mode can be transput.  */

bool
a68_is_transput_mode (MOID_T *p, char rw)
{
  if (p == M_INT)
    return true;
  else if (p == M_SHORT_INT)
    return true;
  else if (p == M_SHORT_SHORT_INT)
    return true;
  else if (p == M_LONG_INT)
    return true;
  else if (p == M_LONG_LONG_INT)
    return true;
  else if (p == M_REAL)
    return true;
  else if (p == M_LONG_REAL)
    return true;
  else if (p == M_LONG_LONG_REAL)
    return true;
  else if (p == M_BOOL)
    return true;
  else if (p == M_CHAR)
    return true;
  else if (p == M_BITS)
    return true;
  else if (p == M_SHORT_BITS)
    return true;
  else if (p == M_SHORT_SHORT_BITS)
    return true;
  else if (p == M_LONG_BITS)
    return true;
  else if (p == M_LONG_LONG_BITS)
    return true;
  else if (p == M_COMPLEX)
    return true;
  else if (p == M_LONG_COMPLEX)
    return true;
  else if (p == M_LONG_LONG_COMPLEX)
    return true;
  else if (p == M_ROW_CHAR)
    return true;
  else if (p == M_STRING)
    return true;
  else if (IS (p, UNION_SYMBOL) || IS (p, STRUCT_SYMBOL))
    {
      for (PACK_T *q = PACK (p); q != NO_PACK; FORWARD (q))
	{
	  if (!(a68_is_transput_mode (MOID (q), rw)
		|| a68_is_proc_ref_file_void_or_format (MOID (q))))
	    return false;
	}
      return true;
    }
  else if (IS_FLEX (p))
    {
      if (SUB (p) == M_ROW_CHAR)
	return true;
      else
	return (rw == 'w' ? a68_is_transput_mode (SUB (p), rw) : false);
    }
  else if (IS_ROW (p))
    return (a68_is_transput_mode (SUB (p), rw)
	    || a68_is_proc_ref_file_void_or_format (SUB (p)));
  else
    return false;
}

/* Whether mode is printable.  */

bool
a68_is_printable_mode (MOID_T *p)
{
  if (a68_is_proc_ref_file_void_or_format (p))
    return true;
  else
    return a68_is_transput_mode (p, 'w');
}

/* Whether mode is readable.  */

bool
a68_is_readable_mode (MOID_T *p)
{
  if (a68_is_proc_ref_file_void_or_format (p))
    return true;
  else if (IS_REF (p))
    return a68_is_transput_mode (SUB (p), 'r');
  else if (IS_UNION (p))
    {
      for (PACK_T *q = PACK (p); q != NO_PACK; FORWARD (q))
	{
	  if (!IS_REF (MOID (q)))
	    return false;
	  else if (!a68_is_transput_mode (SUB (MOID (q)), 'r'))
	    return false;
	}
      return true;
    }
  else
    return false;
}

/* Whether name struct.  */

bool
a68_is_name_struct (MOID_T *p)
{
  return (NAME (p) != NO_MOID ? IS (DEFLEX (SUB (p)), STRUCT_SYMBOL) : false);
}

/* Yield mode to unite to.  */

MOID_T *
a68_unites_to (MOID_T *m, MOID_T *u)
{
  /* Uniting U (m).  */
  MOID_T *v = NO_MOID;

  if (u == M_SIMPLIN || u == M_SIMPLOUT)
    return m;

  for (PACK_T *p = PACK (u); p != NO_PACK; FORWARD (p))
    {
      /* Prefer []->[] over []->FLEX [].  */
      if (m == MOID (p))
	v = MOID (p);
      else if (v == NO_MOID && DEFLEX (m) == DEFLEX (MOID (p)))
	v = MOID (p);
    }
  return v;
}

/* Whether moid in pack.  */

bool
a68_is_moid_in_pack (MOID_T *u, PACK_T *v, int deflex)
{
  for (; v != NO_PACK; FORWARD (v))
    {
      if (a68_is_equal_modes (u, MOID (v), deflex))
	return true;
    }

  return false;
}

/* Whether a rows type in pack.  */

bool
a68_is_rows_in_pack (PACK_T *v)
{
  for (; v != NO_PACK; FORWARD (v))
    {
      if (a68_is_rows_type (MOID (v)))
	return true;
    }

  return false;
}

/* Whether P is a subset of Q.  */

bool
a68_is_subset (MOID_T *p, MOID_T *q, int deflex)
{
  bool j =true;

  for (PACK_T *u = PACK (p); u != NO_PACK && j; FORWARD (u))
    j = (j && a68_is_moid_in_pack (MOID (u), PACK (q), deflex));

  return j;
}

/* Whether P can be united to UNION Q.  */

bool
a68_is_unitable (MOID_T *p, MOID_T *q, int deflex)
{
  if (IS (q, UNION_SYMBOL))
    {
      if (IS (p, UNION_SYMBOL))
	return a68_is_subset (p, q, deflex);
      else if (p == M_ROWS)
	return a68_is_rows_in_pack (PACK (q));
      else
	return a68_is_moid_in_pack (p, PACK (q), deflex);
  }

  return false;
}

/* Whether all or some components of U can be firmly coerced to a component
   mode of V..  */

void
a68_investigate_firm_relations (PACK_T *u, PACK_T *v, bool *all, bool *some)
{
  *all = true;
  *some = true;
  for (; v != NO_PACK; FORWARD (v))
    {
      bool k = false;

      for (PACK_T *w = u; w != NO_PACK; FORWARD (w))
	k |= a68_is_coercible (MOID (w), MOID (v), FIRM, FORCE_DEFLEXING);
      *some |= k;
      *all &= k;
    }
}

/* Whether there is a soft path from P to Q.  */

bool
a68_is_softly_coercible (MOID_T *p, MOID_T *q, int deflex)
{
  if (a68_is_equal_modes (p, q, deflex))
    return true;
  else if (IS (p, PROC_SYMBOL) && PACK (p) == NO_PACK)
    return a68_is_softly_coercible (SUB (p), q, deflex);
  else
    return false;
}

/* Whether there is a weak path from P to Q.  */

bool
a68_is_weakly_coercible (MOID_T * p, MOID_T * q, int deflex)
{
  if (a68_is_equal_modes (p, q, deflex))
    return true;
  else if (a68_is_deprefable (p))
    return a68_is_weakly_coercible (a68_depref_once (p), q, deflex);
  else
    return false;
}

/* Whether there is a meek path from P to Q.  */

bool
a68_is_meekly_coercible (MOID_T *p, MOID_T *q, int deflex)
{
  if (a68_is_equal_modes (p, q, deflex))
    return true;
  else if (a68_is_deprefable (p))
    return a68_is_meekly_coercible (a68_depref_once (p), q, deflex);
  else
    return false;
}

/* Whether there is a firm path from P to Q.  */

bool
a68_is_firmly_coercible (MOID_T *p, MOID_T *q, int deflex)
{
  if (a68_is_equal_modes (p, q, deflex))
    return true;
  else if (q == M_ROWS && a68_is_rows_type (p))
    return true;
  else if (a68_is_unitable (p, q, deflex))
    return true;
  else if (a68_is_deprefable (p))
    return a68_is_firmly_coercible (a68_depref_once (p), q, deflex);
  else
    return false;
}

/* Whether firm.  */

bool
a68_is_firm (MOID_T *p, MOID_T *q)
{
  return (a68_is_firmly_coercible (p, q, SAFE_DEFLEXING)
	  || a68_is_firmly_coercible (q, p, SAFE_DEFLEXING));
}

/* Whether P widens to Q.

   This function returns:

   The destination mode Q if P, or
   Some other mode which is an intermediate step from P to Q, or
   NO_MOID if P cannot be widened to Q.

   This means that if P is known to widen to Q (a68_is_widenable (P,Q) return
   true) this function can be invoked repeteadly and it will eventually return
   Q.  */

MOID_T *
a68_widens_to (MOID_T *p, MOID_T *q)
{
  if (p == M_INT)
    {
      if (q == M_REAL || q == M_COMPLEX)
	{
	  return M_REAL;
	}
      else
	{
	  return NO_MOID;
	}
    }
  else if (p == M_LONG_INT)
    {
      if (q == M_LONG_REAL)
	{
	  return M_LONG_REAL;
	}
      else
	{
	  return NO_MOID;
	}
    }
  else if (p == M_LONG_LONG_INT)
    {
      if (q == M_LONG_LONG_REAL || q == M_LONG_LONG_COMPLEX)
	  return M_LONG_LONG_REAL;
      else
	return NO_MOID;
    }
  else if (p == M_REAL)
    {
      if (q == M_COMPLEX)
	{
	  return M_COMPLEX;
	}
      else
	{
	  return NO_MOID;
	}
    }
  else if (p == M_LONG_REAL)
    {
      if (q == M_LONG_COMPLEX)
	return M_LONG_COMPLEX;
      else
	return NO_MOID;
    }
  else if (p == M_LONG_LONG_REAL)
    {
      if (q == M_LONG_LONG_COMPLEX)
	return M_LONG_LONG_COMPLEX;
      else
	return NO_MOID;
    }
  else if (p == M_BITS)
    {
      if (q == M_ROW_BOOL)
	return M_ROW_BOOL;
      else if (q == M_FLEX_ROW_BOOL)
	return M_FLEX_ROW_BOOL;
      else
	return NO_MOID;
    }
  else if (p == M_SHORT_BITS)
    {
      if (q == M_ROW_BOOL)
	return M_ROW_BOOL;
      else if (q == M_FLEX_ROW_BOOL)
	return M_FLEX_ROW_BOOL;
      else
	return NO_MOID;
    }
  else if (p == M_SHORT_SHORT_BITS)
    {
      if (q == M_ROW_BOOL)
	return M_ROW_BOOL;
      else if (q == M_FLEX_ROW_BOOL)
	return M_FLEX_ROW_BOOL;
      else
	return NO_MOID;
    }
  else if (p == M_LONG_BITS)
    {
      if (q == M_ROW_BOOL)
	return M_ROW_BOOL;
      else if (q == M_FLEX_ROW_BOOL)
	return M_FLEX_ROW_BOOL;
      else
	return NO_MOID;
    }
  else if (p == M_LONG_LONG_BITS)
    {
      if (q == M_ROW_BOOL)
	return M_ROW_BOOL;
      else if (q == M_FLEX_ROW_BOOL)
	return M_FLEX_ROW_BOOL;
      else
	return NO_MOID;
    }
  else if (p == M_BYTES && q == M_ROW_CHAR)
    return M_ROW_CHAR;
  else if (p == M_LONG_BYTES && q == M_ROW_CHAR)
    return M_ROW_CHAR;
  else if (p == M_BYTES && q == M_FLEX_ROW_CHAR)
    return M_FLEX_ROW_CHAR;
  else if (p == M_LONG_BYTES && q == M_FLEX_ROW_CHAR)
    return M_FLEX_ROW_CHAR;
  else
    return NO_MOID;
}

/* Whether P widens to Q.  */

bool
a68_is_widenable (MOID_T *p, MOID_T *q)
{
  MOID_T *z = a68_widens_to (p, q);

  if (z != NO_MOID)
    return (z == q ? true : a68_is_widenable (z, q));
  else
    return false;
}

/* Whether P is a REF ROW.  */

bool
a68_is_ref_row (MOID_T *p)
{
  return (NAME (p) != NO_MOID ? IS_ROW (DEFLEX (SUB (p))) : false);
}

/* Whether strong name.  */

bool
a68_is_strong_name (MOID_T *p, MOID_T *q)
{
  if (p == q)
    return true;
  else if (a68_is_ref_row (q))
    return a68_is_strong_name (p, NAME (q));
  else
    return false;
}

/* Whether strong slice. */

bool
a68_is_strong_slice (MOID_T *p, MOID_T *q)
{
  if (p == q || a68_is_widenable (p, q))
    return true;
  else if (SLICE (q) != NO_MOID)
    return a68_is_strong_slice (p, SLICE (q));
  else if (IS_FLEX (q))
    return a68_is_strong_slice (p, SUB (q));
  else if (a68_is_ref_row (q))
    return a68_is_strong_name (p, q);
  else
    return false;
}

/* Whether strongly coercible.  */

bool
a68_is_strongly_coercible (MOID_T *p, MOID_T *q, int deflex)
{
  /* Keep this sequence of statements.  */
  if (a68_is_equal_modes (p, q, deflex))
    return true;
  else if (q == M_VOID)
    return true;
  else if ((q == M_SIMPLIN || q == M_ROW_SIMPLIN) && a68_is_readable_mode (p))
    return true;
  else if (q == M_ROWS && a68_is_rows_type (p))
    return true;
  else if (a68_is_unitable (p, a68_derow (q), deflex))
    return true;

  if (a68_is_ref_row (q) && a68_is_strong_name (p, q))
    return true;
  else if (SLICE (q) != NO_MOID && a68_is_strong_slice (p, q))
    return true;
  else if (IS_FLEX (q) && a68_is_strong_slice (p, q))
    return true;
  else if (a68_is_widenable (p, q))
    return true;
  else if (a68_is_deprefable (p))
    return a68_is_strongly_coercible (a68_depref_once (p), q, deflex);
  else if (q == M_SIMPLOUT || q == M_ROW_SIMPLOUT)
    return a68_is_printable_mode (p);
  else
    return false;
}

/* Basic coercions.  */

bool
a68_basic_coercions (MOID_T *p, MOID_T *q, int c, int deflex)
{
  if (a68_is_equal_modes (p, q, deflex))
    return true;
  else if (c == NO_SORT)
    return (p == q);
  else if (c == SOFT)
    return a68_is_softly_coercible (p, q, deflex);
  else if (c == WEAK)
    return a68_is_weakly_coercible (p, q, deflex);
  else if (c == MEEK)
    return a68_is_meekly_coercible (p, q, deflex);
  else if (c == FIRM)
    return a68_is_firmly_coercible (p, q, deflex);
  else if (c == STRONG)
    return a68_is_strongly_coercible (p, q, deflex);
  else
    return false;
}

/* Whether coercible stowed.  */

bool
a68_is_coercible_stowed (MOID_T *p, MOID_T *q, int c, int deflex)
{
  if (c != STRONG)
    /* Such construct is always in a strong position, is it not?  */
    return false;
  else if (q == M_VOID)
    return true;
  else if (IS_FLEX (q))
    {
      bool j = true;

      for (PACK_T *u = PACK (p); u != NO_PACK && j; FORWARD (u))
	j &= a68_is_coercible (MOID (u), SLICE (SUB (q)), c, deflex);
      return j;
    }
  else if (IS_ROW (q))
    {
      bool j = true;

      for (PACK_T *u = PACK (p); u != NO_PACK && j; FORWARD (u))
	j &= a68_is_coercible (MOID (u), SLICE (q), c, deflex);
      return j;
    }
  else if (IS (q, PROC_SYMBOL) || IS (q, STRUCT_SYMBOL))
    {
      if (DIM (p) != DIM (q))
	return false;
      else
	{
	  PACK_T *u = PACK (p), *v = PACK (q);
	  bool j = true;

	  while (u != NO_PACK && v != NO_PACK && j)
	    {
	      j &= a68_is_coercible (MOID (u), MOID (v), c, deflex);
	      FORWARD (u);
	      FORWARD (v);
	    }
	  return j;
	}
    }
  else
    return false;
}

/* Whether coercible series.  */

bool
a68_is_coercible_series (MOID_T *p, MOID_T *q, int c, int deflex)
{
  if (c == NO_SORT)
    return false;
  else if (p == NO_MOID || q == NO_MOID)
    return false;
  else if (IS (p, SERIES_MODE) && PACK (p) == NO_PACK)
    return false;
  else if (IS (q, SERIES_MODE) && PACK (q) == NO_PACK)
    return false;
  else if (PACK (p) == NO_PACK)
    return a68_is_coercible (p, q, c, deflex);
  else
    {
      bool j = true;

      for (PACK_T *u = PACK (p); u != NO_PACK && j; FORWARD (u))
	{
	  if (MOID (u) != NO_MOID)
	    j &= a68_is_coercible (MOID (u), q, c, deflex);
	}
    return j;
    }
}

/* Whether P can be coerced to Q in a C context.

   If P is a STOWED modes serie (A, B, ...) and Q is a routine mode like `proc
   (X, Y, ...)' then this routine determines whether A can be coerced to X, B
   to Y, etc.  */

bool
a68_is_coercible (MOID_T *p, MOID_T *q, int c, int deflex)
{
  if (a68_is_mode_isnt_well (p) || a68_is_mode_isnt_well (q))
    return true;
  else if (a68_is_equal_modes (p, q, deflex))
    return true;
  else if (p == M_HIP)
    return true;
  else if (IS (p, STOWED_MODE))
    return a68_is_coercible_stowed (p, q, c, deflex);
  else if (IS (p, SERIES_MODE))
    return a68_is_coercible_series (p, q, c, deflex);
  else if (p == M_VACUUM && IS_ROW (DEFLEX (q)))
    return true;
  else
    return a68_basic_coercions (p, q, c, deflex);
}

/* Whether coercible in context.  */

bool
a68_is_coercible_in_context (SOID_T *p, SOID_T *q, int deflex)
{
  if (SORT (p) != SORT (q))
    return false;
  else if (MOID (p) == MOID (q))
    return true;
  else
    return a68_is_coercible (MOID (p), MOID (q), SORT (q), deflex);
}

/* Whether list Y is balanced.  */

bool
a68_is_balanced (NODE_T *n, SOID_T *y, int sort)
{
  if (sort == STRONG)
    return true;
  else
    {
      bool k = false;

      for (; y != NO_SOID && !k; FORWARD (y)) 
	k = (!IS (MOID (y), STOWED_MODE));

      if (k == false)
	a68_error (n, "construct has no unique mode");
      return k;
    }
}

/* A moid from M to which all other members can be coerced.
   If no fulcrum of the balance is found, return NO_MOID.  */

MOID_T *
a68_get_balanced_mode_or_no_mode (MOID_T *m, int sort, bool return_depreffed, int deflex)
{
  MOID_T *common_moid = NO_MOID;

  if (m != NO_MOID && !a68_is_mode_isnt_well (m) && IS (m, UNION_SYMBOL))
    {
      int depref_level;
      bool siga = true;
      /* Test for increasing depreffing.  */
      for (depref_level = 0; siga; depref_level++)
	{
	  siga = false;
	  /* Test the whole pack.  */
	  for (PACK_T *p = PACK (m); p != NO_PACK; FORWARD (p))
	    {
	      /* HIPs are not eligible of course.  */
	      if (MOID (p) != M_HIP)
		{
		  MOID_T *candidate = MOID (p);
		  int k;
		  /* Depref as far as allowed.  */
		  for (k = depref_level; k > 0 && a68_is_deprefable (candidate); k--)
		    candidate = a68_depref_once (candidate);
		  /* Only need testing if all allowed deprefs succeeded.  */
		  if (k == 0)
		    {
		      MOID_T *to = (return_depreffed ? a68_depref_completely (candidate) : candidate);
		      bool all_coercible = true;

		      siga = true;
		      for (PACK_T *q = PACK (m); q != NO_PACK && all_coercible; FORWARD (q))
			{
			  MOID_T *from = MOID (q);
			  if (p != q && from != to)
			    all_coercible &= a68_is_coercible (from, to, sort, deflex);
			}
		      /* If the pack is coercible to the candidate, we mark the
			 candidate.  We continue searching for longest series
			 of REF REF PROC REF.  */
		      if (all_coercible)
			{
			  MOID_T *mark = (return_depreffed ? MOID (p) : candidate);

			  if (common_moid == NO_MOID)
			    common_moid = mark;
			  else if (IS_FLEX (candidate) && DEFLEX (candidate) == common_moid)
			    /* We prefer FLEX.  */
			    common_moid = mark;
			}
		    }
		}
	    }
	}
    }

  return common_moid;
}

/* A moid from M to which all other members can be coerced.
   If no fulcrum of the balance is found, return M.  */

MOID_T *
a68_get_balanced_mode (MOID_T *m, int sort, bool return_depreffed, int deflex)
{
  MOID_T *common_moid
    = a68_get_balanced_mode_or_no_mode (m, sort, return_depreffed, deflex);
  return common_moid == NO_MOID ? m : common_moid;
}

/* Whether we can search a common mode from a clause or not.  */

bool
a68_clause_allows_balancing (int att)
{
  switch (att)
    {
    case CLOSED_CLAUSE:
    case CONDITIONAL_CLAUSE:
    case CASE_CLAUSE:
    case SERIAL_CLAUSE:
    case CONFORMITY_CLAUSE:
      return true;
    }
  return false;
}

/* A unique mode from Z.  */

MOID_T *
a68_determine_unique_mode (SOID_T *z, int deflex)
{
  if (z == NO_SOID)
    return NO_MOID;
  else
    {
      MOID_T *x = MOID (z);

      if (a68_is_mode_isnt_well (x))
	return M_ERROR;

      /* If X is a series containing one union, a68_make_united_mode will
	 return that union (because 'union (union (...))' is the same than
	 'union (...)') and then a68_get_balanced_mode below will try to
	 balance the modes in that union.  Not what we want.  */
      if (ATTRIBUTE (x) == SERIES_MODE
	  && DIM (x) == 1
	  && IS (MOID (PACK (x)), UNION_SYMBOL))
	return MOID (PACK (x));

      x = a68_make_united_mode (x);
      if (a68_clause_allows_balancing (ATTRIBUTE (z)))
	return a68_get_balanced_mode (x, STRONG, A68_NO_DEPREF, deflex);
      else
	return x;
    }
}

/* Insert coercion A in the tree.  */

void
a68_make_coercion (NODE_T *l, enum a68_attribute a, MOID_T *m)
{
  a68_make_sub (l, l, a);
  MOID (l) = a68_depref_rows (MOID (l), m);
}

/* Make widening coercion.  */

static void
make_widening_coercion (NODE_T *n, MOID_T *p, MOID_T *q)
{
  MOID_T *z = a68_widens_to (p, q);

  a68_make_coercion (n, WIDENING, z);
  if (z != q)
    make_widening_coercion (n, z, q);
}

/* Make ref rowing coercion.  */

void
a68_make_ref_rowing_coercion (NODE_T *n, MOID_T *p, MOID_T *q)
{
  if (DEFLEX (p) != DEFLEX (q))
    {
      if (a68_is_widenable (p, q))
	make_widening_coercion (n, p, q);
      else if (a68_is_ref_row (q))
	{
	  a68_make_ref_rowing_coercion (n, p, NAME (q));
	  a68_make_coercion (n, ROWING, q);
	}
    }
}

/* Make rowing coercion.  */

void
a68_make_rowing_coercion (NODE_T *n, MOID_T *p, MOID_T *q)
{
  if (DEFLEX (p) != DEFLEX (q))
    {
      if (a68_is_widenable (p, q))
	make_widening_coercion (n, p, q);
      else if (SLICE (q) != NO_MOID)
	{
	  a68_make_rowing_coercion (n, p, SLICE (q));
	  a68_make_coercion (n, ROWING, q);
	}
      else if (IS_FLEX (q))
	a68_make_rowing_coercion (n, p, SUB (q));
      else if (a68_is_ref_row (q))
	a68_make_ref_rowing_coercion (n, p, q);
    }
}

/* Make uniting coercion.  */

void
a68_make_uniting_coercion (NODE_T *n, MOID_T *q)
{
  a68_make_coercion (n, UNITING, a68_derow (q));
  if (IS_ROW (q) || IS_FLEX (q))
    a68_make_rowing_coercion (n, a68_derow (q), q);
}

/* Make depreffing coercion to coerce node N from mode P to mode Q in a strong
   context.  */

void
a68_make_depreffing_coercion (NODE_T *n, MOID_T *p, MOID_T *q)
{
  if (DEFLEX (p) == DEFLEX (q))
    return;
  else if (q == M_SIMPLOUT && a68_is_printable_mode (p))
    a68_make_coercion (n, UNITING, q);
  else if (q == M_ROW_SIMPLOUT && a68_is_printable_mode (p))
    {
      a68_make_coercion (n, UNITING, M_SIMPLOUT);
      a68_make_coercion (n, ROWING, M_ROW_SIMPLOUT);
    }
  else if (q == M_SIMPLIN && a68_is_readable_mode (p))
    a68_make_coercion (n, UNITING, q);
  else if (q == M_ROW_SIMPLIN && a68_is_readable_mode (p))
    {
      a68_make_coercion (n, UNITING, M_SIMPLIN);
      a68_make_coercion (n, ROWING, M_ROW_SIMPLIN);
    }
  else if (q == M_ROWS && a68_is_rows_type (p))
    {
      a68_make_coercion (n, UNITING, M_ROWS);
      MOID (n) = M_ROWS;
    }
  else if (a68_is_widenable (p, q))
    make_widening_coercion (n, p, q);
  else if (a68_is_unitable (p, a68_derow (q), SAFE_DEFLEXING))
    a68_make_uniting_coercion (n, q);
  else if (a68_is_ref_row (q) && a68_is_strong_name (p, q))
    a68_make_ref_rowing_coercion (n, p, q);
  else if (SLICE (q) != NO_MOID && a68_is_strong_slice (p, q))
    a68_make_rowing_coercion (n, p, q);
  else if (IS_FLEX (q) && a68_is_strong_slice (p, q))
    a68_make_rowing_coercion (n, p, q);
  else if (IS_REF (p))
    {
      MOID_T *r = a68_depref_once (p);
      a68_make_coercion (n, DEREFERENCING, r);
      a68_make_depreffing_coercion (n, r, q);
    }
  else if (IS (p, PROC_SYMBOL) && PACK (p) == NO_PACK)
    {
      MOID_T *r = SUB (p);

      a68_make_coercion (n, DEPROCEDURING, r);
      a68_make_depreffing_coercion (n, r, q);
    }
  else if (p != q)
    a68_cannot_coerce (n, p, q, NO_SORT, SKIP_DEFLEXING, 0);
}

/* Whether p is a nonproc mode (that is voided directly).  */

bool
a68_is_nonproc (MOID_T *p)
{
  if (IS (p, PROC_SYMBOL) && PACK (p) == NO_PACK)
    return false;
  else if (IS_REF (p))
    return a68_is_nonproc (SUB (p));
  else
    return true;
}

/* Voiden in an appropriate way.  */

void
a68_make_void (NODE_T *p, MOID_T *q)
{
  switch (ATTRIBUTE (p))
    {
    case ASSIGNATION:
    case IDENTITY_RELATION:
    case GENERATOR:
    case CAST:
    case DENOTATION:
      a68_make_coercion (p, VOIDING, M_VOID);
      return;
    default:
      break;
    }

  /* MORFs are an involved case.  */
  switch (ATTRIBUTE (p))
    {
    case SELECTION:
    case SLICE:
    case ROUTINE_TEXT:
    case FORMULA:
    case CALL:
    case IDENTIFIER:
      /* A nonproc moid value is eliminated directly.  */
      if (a68_is_nonproc (q))
	{
	  a68_make_coercion (p, VOIDING, M_VOID);
	  return;
	}
      else
	{
	  /* Descend the chain of e.g. REF PROC .. until a nonproc moid
	     remains.  */
	  MOID_T *z = q;

	  while (!a68_is_nonproc (z))
	    {
	      if (IS_REF (z))
		a68_make_coercion (p, DEREFERENCING, SUB (z));
	      if (IS (z, PROC_SYMBOL) && NODE_PACK (p) == NO_PACK)
		a68_make_coercion (p, DEPROCEDURING, SUB (z));
	      z = SUB (z);
	    }
	  if (z != M_VOID)
	    a68_make_coercion (p, VOIDING, M_VOID);
	  return;
	}
    default:
      break;
    }

  /* All other is voided straight away.  */
  a68_make_coercion (p, VOIDING, M_VOID);
}

/* Make strong coercion of node N from mode P to mode Q.  */

void
a68_make_strong (NODE_T *n, MOID_T *p, MOID_T *q)
{
  if (q == M_VOID && p != M_VOID)
    a68_make_void (n, p);
  else
    a68_make_depreffing_coercion (n, p, q);
}
