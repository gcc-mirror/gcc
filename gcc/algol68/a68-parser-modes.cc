/* Mode table management.
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
 * Mode collection, equivalencing and derived modes.
 */

/* Few forward references.  */

static MOID_T *get_mode_from_declarer (NODE_T *p);

/*
 * Mode service routines.
 */

/* Count bounds in declarer in tree.  */

static int
count_bounds (NODE_T *p)
{
  if (p == NO_NODE)
    return 0;
  else
    {
      if (IS (p, BOUND))
	return 1 + count_bounds (NEXT (p));
      else
	return count_bounds (NEXT (p)) + count_bounds (SUB (p));
    }
}

/* Count number of SHORTs or LONGs. */

static int
count_sizety (NODE_T *p)
{
  if (p == NO_NODE)
    return 0;
  else if (IS (p, LONGETY))
    return count_sizety (SUB (p)) + count_sizety (NEXT (p));
  else if (IS (p, SHORTETY))
    return count_sizety (SUB (p)) + count_sizety (NEXT (p));
  else if (IS (p, LONG_SYMBOL))
    return 1;
  else if (IS (p, SHORT_SYMBOL))
    return -1;
  else
    return 0;
}

/* Count moids in a pack.  */

int
a68_count_pack_members (PACK_T *u)
{
  int k = 0;

  for (; u != NO_PACK; FORWARD (u))
    k++;
  return k;
}

/* Replace a mode by its equivalent mode.  */

static void
resolve_equivalent (MOID_T **m)
{
  while ((*m) != NO_MOID
	 && EQUIVALENT ((*m)) != NO_MOID
	 && (*m) != EQUIVALENT (*m))
    {
      (*m) = EQUIVALENT (*m);
    }
}

/* Reset moid.  */

static void
reset_moid_tree (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      MOID (p) = NO_MOID;
      reset_moid_tree (SUB (p));
    }
}

/* Renumber moids.  */

void
a68_renumber_moids (MOID_T *p, int n)
{
  if (p != NO_MOID)
    {
      NUMBER (p) = n;
      a68_renumber_moids (NEXT (p), n + 1);
    }
}

/* See whether a mode equivalent to the mode M exists in the global mode table,
   and return it.  Return NO_MOID if no equivalent mode is found.  */

MOID_T *
a68_search_equivalent_mode (MOID_T *m)
{
  for (MOID_T *head = TOP_MOID (&A68_JOB); head != NO_MOID; FORWARD (head))
    {
      if (a68_prove_moid_equivalence (head, m))
	return head;
    }

  return NO_MOID;
}

/* Register mode in the global mode table, if mode is unique.  */

MOID_T *
a68_register_extra_mode (MOID_T **z, MOID_T *u)
{
  /* If we already know this mode, return the existing entry; otherwise link it
     in.  */
  for (MOID_T *head = TOP_MOID (&A68_JOB); head != NO_MOID; FORWARD (head))
    {
      if (a68_prove_moid_equivalence (head, u))
	return head;
    }

  /* Link to chain and exit.  */
  NUMBER (u) = A68 (mode_count)++;
  NEXT (u) = (*z);
  return *z = u;
}

/* Create a new mode.  */

MOID_T *
a68_create_mode (int att, int dim, NODE_T *node, MOID_T *sub, PACK_T *pack)
{
  MOID_T *new_mode = a68_new_moid ();

  if (sub == NO_MOID)
    {
      if (att == REF_SYMBOL
	  || att == FLEX_SYMBOL
	  || att == ROW_SYMBOL)
	gcc_unreachable ();
    }

  USE (new_mode) = false;
  ATTRIBUTE (new_mode) = att;
  DIM (new_mode) = dim;
  NODE (new_mode) = node;
  HAS_ROWS (new_mode) = (att == ROW_SYMBOL);
  SUB (new_mode) = sub;
  PACK (new_mode) = pack;
  NEXT (new_mode) = NO_MOID;
  EQUIVALENT (new_mode) = NO_MOID;
  SLICE (new_mode) = NO_MOID;
  DEFLEXED (new_mode) = NO_MOID;
  NAME (new_mode) = NO_MOID;
  MULTIPLE (new_mode) = NO_MOID;
  ROWED (new_mode) = NO_MOID;

  return new_mode;
}

/* Create a new mode and add it to chain Z.  */

MOID_T *
a68_add_mode (MOID_T **z, int att, int dim, NODE_T *node, MOID_T *sub, PACK_T *pack)
{
  MOID_T *new_mode = a68_create_mode (att, dim, node, sub, pack);
  return a68_register_extra_mode (z, new_mode);
}

/* Contract a UNION.  */

void
a68_contract_union (MOID_T *u)
{
  for (PACK_T *s = PACK (u); s != NO_PACK; FORWARD (s))
    {
      PACK_T *t = s;

      while (t != NO_PACK)
	{
	  if (NEXT (t) != NO_PACK && MOID (NEXT (t)) == MOID (s))
	    {
	      MOID (t) = MOID (t);
	      NEXT (t) = NEXT_NEXT (t);
	    }
	  else
	    FORWARD (t);
	}
    }
}

/* Absorb UNION pack.  */

PACK_T *
a68_absorb_union_pack (PACK_T * u)
{
  PACK_T *z;
  bool siga;

  do
    {
      z = NO_PACK;
      siga = false;
      for (PACK_T *t = u; t != NO_PACK; FORWARD (t))
	{
	  if (IS (MOID (t), UNION_SYMBOL))
	    {
	      siga = true;
	      for (PACK_T *s = PACK (MOID (t)); s != NO_PACK; FORWARD (s))
		(void) a68_add_mode_to_pack (&z, MOID (s), NO_TEXT, NODE (s));
	    }
	  else
	    {
	      (void) a68_add_mode_to_pack (&z, MOID (t), NO_TEXT, NODE (t));
	    }
	}
      u = z;
    }
  while (siga);
  return z;
}

/* Add row and its slices to chain, recursively.  */

static MOID_T *
add_row (MOID_T **p, int dim, MOID_T *sub, NODE_T *n, bool derivate)
{
  MOID_T *q = a68_add_mode (p, ROW_SYMBOL, dim, n, sub, NO_PACK);

  DERIVATE (q) |= derivate;
  if (dim > 1)
    SLICE (q) = add_row (&NEXT (q), dim - 1, sub, n, derivate);
  else
    SLICE (q) = sub;
  return q;
}

/* Add a moid to a pack, maybe with a (field) name.  */

void
a68_add_mode_to_pack (PACK_T **p, MOID_T *m, const char *text, NODE_T *node)
{
  PACK_T *z = a68_new_pack ();

  MOID (z) = m;
  TEXT (z) = text;
  NODE (z) = node;
  NEXT (z) = *p;
  PREVIOUS (z) = NO_PACK;
  if (NEXT (z) != NO_PACK)
    PREVIOUS (NEXT (z)) = z;

  /* Link in chain.  */
  *p = z;
}

/* Add a moid to a pack, maybe with a (field) name.  */

void
a68_add_mode_to_pack_end (PACK_T **p, MOID_T *m, const char *text, NODE_T *node)
{
  PACK_T *z = a68_new_pack ();

  MOID (z) = m;
  TEXT (z) = text;
  NODE (z) = node;
  NEXT (z) = NO_PACK;
  if (NEXT (z) != NO_PACK)
    PREVIOUS (NEXT (z)) = z;

  /* Link in chain.  */
  while ((*p) != NO_PACK)
    p = &(NEXT (*p));
  PREVIOUS (z) = (*p);
  (*p) = z;
}

/* Absorb UNION members.  */

static void
absorb_unions (MOID_T *m)
{
  /* UNION (A, UNION (B, C)) = UNION (A, B, C) or
     UNION (A, UNION (A, B)) = UNION (A, B).  */
  for (; m != NO_MOID; FORWARD (m))
    {
      if (IS (m, UNION_SYMBOL))
	PACK (m) = a68_absorb_union_pack (PACK (m));
    }
}

/* Contract UNIONs.  */

static void
contract_unions (MOID_T *m)
{
  /* UNION (A, B, A) -> UNION (A, B).  */
  for (; m != NO_MOID; FORWARD (m))
    {
      if (IS (m, UNION_SYMBOL) && EQUIVALENT (m) == NO_MOID)
	a68_contract_union (m);
    }
}

/*
 * Routines to collect MOIDs from the program text.
 */

/* Search standard mode in standard environ.  */

static MOID_T *
search_standard_mode (int sizety, NODE_T *indicant)
{
  /* Search standard mode.  */
  for (MOID_T *p = TOP_MOID (&A68_JOB); p != NO_MOID; FORWARD (p))
    {
      if (IS (p, STANDARD)
	  && DIM (p) == sizety
	  && NSYMBOL (NODE (p)) == NSYMBOL (indicant))
	return p;
  }

  /* Map onto greater precision.  */
  if (sizety < 0)
    return search_standard_mode (sizety + 1, indicant);
  else if (sizety > 0)
    return search_standard_mode (sizety - 1, indicant);
  else
    return NO_MOID;
}

/* Collect mode from STRUCT field.  */

static void
get_mode_from_struct_field (NODE_T *p, PACK_T **u)
{
  if (p != NO_NODE)
    {
      if (IS (p, IDENTIFIER))
	{
	  ATTRIBUTE (p) = FIELD_IDENTIFIER;
	  (void) a68_add_mode_to_pack (u, NO_MOID, NSYMBOL (p), p);
	}
      else if (IS (p, DECLARER))
	{
	  MOID_T *new_one = get_mode_from_declarer (p);

	  get_mode_from_struct_field (NEXT (p), u);
	  for (PACK_T *t = *u; t && MOID (t) == NO_MOID; FORWARD (t))
	    {
	      MOID (t) = new_one;
	      MOID (NODE (t)) = new_one;
	    }
	}
      else
	{
	  get_mode_from_struct_field (NEXT (p), u);
	  get_mode_from_struct_field (SUB (p), u);
	}
    }
}

/* Collect MODE from formal pack.  */

static void
get_mode_from_formal_pack (NODE_T *p, PACK_T **u)
{
  if (p != NO_NODE)
    {
      if (IS (p, DECLARER))
	{
	  get_mode_from_formal_pack (NEXT (p), u);
	  MOID_T *z = get_mode_from_declarer (p);
	  (void) a68_add_mode_to_pack (u, z, NO_TEXT, p);
	}
      else
	{
	  get_mode_from_formal_pack (NEXT (p), u);
	  get_mode_from_formal_pack (SUB (p), u);
	}
    }
}

/* Collect MODE or VOID from formal UNION pack.  */

static void
get_mode_from_union_pack (NODE_T *p, PACK_T **u)
{
  if (p != NO_NODE)
    {
      if (IS (p, DECLARER) || IS (p, VOID_SYMBOL))
	{
	  get_mode_from_union_pack (NEXT (p), u);
	  MOID_T *z = get_mode_from_declarer (p);
	  (void) a68_add_mode_to_pack (u, z, NO_TEXT, p);
	}
      else
	{
	  get_mode_from_union_pack (NEXT (p), u);
	  get_mode_from_union_pack (SUB (p), u);
	}
    }
}

/* Collect mode from PROC, OP pack.  */

static void
get_mode_from_routine_pack (NODE_T *p, PACK_T **u)
{
  if (p != NO_NODE)
    {
      if (IS (p, IDENTIFIER))
	(void) a68_add_mode_to_pack (u, NO_MOID, NO_TEXT, p);
      else if (IS (p, DECLARER))
	{
	  MOID_T *z = get_mode_from_declarer (p);

	  for (PACK_T *t = *u; t != NO_PACK && MOID (t) == NO_MOID; FORWARD (t))
	    {
	      MOID (t) = z;
	      MOID (NODE (t)) = z;
	    }
	  (void) a68_add_mode_to_pack (u, z, NO_TEXT, p);
	}
      else
	{
	  get_mode_from_routine_pack (NEXT (p), u);
	  get_mode_from_routine_pack (SUB (p), u);
	}
    }
}

/* Collect MODE from DECLARER.  */

static MOID_T *
get_mode_from_declarer (NODE_T *p)
{
  if (p == NO_NODE)
    return NO_MOID;
  else
    {
      if (IS (p, DECLARER))
	{
	  if (MOID (p) != NO_MOID)
	    return MOID (p);
	 else
	   return MOID (p) = get_mode_from_declarer (SUB (p));
	}
      else
	{
	  if (IS (p, VOID_SYMBOL))
	    {
	      MOID (p) = M_VOID;
	      return MOID (p);
	    }
	  else if (IS (p, LONGETY))
	    {
	      if (a68_whether (p, LONGETY, INDICANT, STOP))
		{
		  int k = count_sizety (SUB (p));
		  MOID (p) = search_standard_mode (k, NEXT (p));
		  return MOID (p);
		}
	      else
		{
		  return NO_MOID;
		}
	    }
	  else if (IS (p, SHORTETY))
	    {
	      if (a68_whether (p, SHORTETY, INDICANT, STOP))
		{
		  int k = count_sizety (SUB (p));
		  MOID (p) = search_standard_mode (k, NEXT (p));
		  return MOID (p);
		}
	      else
		return NO_MOID;
	    }
	  else if (IS (p, INDICANT))
	    {
	      MOID_T *q = search_standard_mode (0, p);
	      if (q != NO_MOID)
		  MOID (p) = q;
	      else
		{
		  /* Position of definition tells indicants apart.  */
		  TAG_T *y = a68_find_tag_global (TABLE (p), INDICANT, NSYMBOL (p));
		  if (y == NO_TAG)
		    a68_error ( p, "tag Z has not been declared properly", NSYMBOL (p));
		  else
		    MOID (p) = a68_add_mode (&TOP_MOID (&A68_JOB), INDICANT, 0, NODE (y),
					     NO_MOID, NO_PACK);
		}
	      return MOID (p);
	    }
	  else if (IS_REF (p))
	    {
	      MOID_T *new_one = get_mode_from_declarer (NEXT (p));
	      MOID (p) = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, p, new_one, NO_PACK);
	      return MOID (p);
	    }
	  else if (IS_FLEX (p))
	    {
	      MOID_T *new_one = get_mode_from_declarer (NEXT (p));
	      MOID (p) = a68_add_mode (&TOP_MOID (&A68_JOB), FLEX_SYMBOL, 0, p, new_one, NO_PACK);
	      SLICE (MOID (p)) = SLICE (new_one);
	      return MOID (p);
	    }
	  else if (IS (p, FORMAL_BOUNDS))
	    {
	      MOID_T *new_one = get_mode_from_declarer (NEXT (p));
	      MOID (p) = add_row (&TOP_MOID (&A68_JOB),
				  1 + a68_count_formal_bounds (SUB (p)), new_one, p, false);
	      return MOID (p);
	    }
	  else if (IS (p, BOUNDS))
	    {
	      MOID_T *new_one = get_mode_from_declarer (NEXT (p));
	      MOID (p) = add_row (&TOP_MOID (&A68_JOB), count_bounds (SUB (p)), new_one, p, false);
	      return MOID (p);
	    }
	  else if (IS (p, STRUCT_SYMBOL))
	    {
	      PACK_T *u = NO_PACK;
	      get_mode_from_struct_field (NEXT (p), &u);
	      MOID (p) = a68_add_mode (&TOP_MOID (&A68_JOB),
				       STRUCT_SYMBOL, a68_count_pack_members (u), p, NO_MOID, u);
	      return MOID (p);
	    }
	  else if (IS (p, UNION_SYMBOL))
	    {
	      PACK_T *u = NO_PACK;
	      get_mode_from_union_pack (NEXT (p), &u);
	      MOID (p) = a68_add_mode (&TOP_MOID (&A68_JOB),
				       UNION_SYMBOL, a68_count_pack_members (u), p, NO_MOID, u);
	      return MOID (p);
	    }
	  else if (IS (p, PROC_SYMBOL))
	    {
	      NODE_T *save = p;
	      PACK_T *u = NO_PACK;
	      if (IS (NEXT (p), FORMAL_DECLARERS))
		{
		  get_mode_from_formal_pack (SUB_NEXT (p), &u);
		  FORWARD (p);
		}
	      MOID_T *new_one = get_mode_from_declarer (NEXT (p));
	      MOID (p) =
		a68_add_mode (&TOP_MOID (&A68_JOB), PROC_SYMBOL, a68_count_pack_members (u), save, new_one, u);
	      MOID (save) = MOID (p);
	      return MOID (p);
	    }
	  else
	    return NO_MOID;
	}
    }
}

/* Collect MODEs from a routine-text header.  */

static MOID_T *
get_mode_from_routine_text (NODE_T *p)
{
  PACK_T *u = NO_PACK;
  NODE_T *q = p;

  if (IS (p, PARAMETER_PACK))
    {
      get_mode_from_routine_pack (SUB (p), &u);
      FORWARD (p);
    }
  MOID_T *n = get_mode_from_declarer (p);
  return a68_add_mode (&TOP_MOID (&A68_JOB), PROC_SYMBOL, a68_count_pack_members (u), q, n, u);
}

/* Collect modes from operator-plan.  */

static MOID_T *
get_mode_from_operator (NODE_T *p)
{
  PACK_T *u = NO_PACK;
  NODE_T *save = p;

  if (IS (NEXT (p), FORMAL_DECLARERS))
    {
      get_mode_from_formal_pack (SUB_NEXT (p), &u);
      FORWARD (p);
    }
  MOID_T *new_one = get_mode_from_declarer (NEXT (p));
  MOID (p) = a68_add_mode (&TOP_MOID (&A68_JOB), PROC_SYMBOL, a68_count_pack_members (u), save, new_one, u);
  return MOID (p);
}

/* Collect mode from denotation.  */

static void
get_mode_from_denotation (NODE_T *p, int sizety)
{
  if (p != NO_NODE)
    {
      if (IS (p, ROW_CHAR_DENOTATION))
	{
	  const char *s = NSYMBOL (p);
	  size_t len = strlen (s);

	  if (len == 1
	      || (len == 2 && s[0] == '\'')
	      || (len == 8 && s[0] == '\'' && s[1] == '(' && s[2] == 'u')
	      || (len == 12 && s[0] == '\'' && s[1] == '(' && s[2] == 'U'))
	    {
	      MOID (p) = M_CHAR;
	    }
	  else
	    MOID (p) = M_ROW_CHAR;
	}
      else if (IS (p, TRUE_SYMBOL) || IS (p, FALSE_SYMBOL))
	{
	  MOID (p) = M_BOOL;
	}
      else if (IS (p, INT_DENOTATION))
	{
	  if (sizety == -2)
	    MOID (p) = M_SHORT_SHORT_INT;
	  else if (sizety == -1)
	    MOID (p) = M_SHORT_INT;
	  else if (sizety == 0)
	    MOID (p) = M_INT;
	  else if (sizety == 1)
	    MOID (p) = M_LONG_INT;
	  else if (sizety == 2)
	    MOID (p) = M_LONG_LONG_INT;
	 else
	   MOID (p) = (sizety > 0 ? M_LONG_LONG_INT : M_INT);
	}
      else if (IS (p, REAL_DENOTATION))
	{
	  if (sizety == 0)
	    MOID (p) = M_REAL;
	  else if (sizety == 1)
	    MOID (p) = M_LONG_REAL;
	 else if (sizety == 2)
	   MOID (p) = M_LONG_LONG_REAL;
	 else
	   MOID (p) = (sizety > 0 ? M_LONG_LONG_REAL : M_REAL);
	}
      else if (IS (p, BITS_DENOTATION))
	{
	  if (sizety == -2)
	    MOID (p) = M_SHORT_SHORT_BITS;
	  else if (sizety == -1)
	    MOID (p) = M_SHORT_BITS;
	  else if (sizety == 0)
	    MOID (p) = M_BITS;
	  else if (sizety == 1)
	    MOID (p) = M_LONG_BITS;
	  else if (sizety == 2)
	    MOID (p) = M_LONG_LONG_BITS;
	  else
	    MOID (p) = (sizety > 0 ? M_LONG_LONG_BITS : M_BITS);
	}
      else if (IS (p, LONGETY) || IS (p, SHORTETY))
	{
	  get_mode_from_denotation (NEXT (p), count_sizety (SUB (p)));
	  MOID (p) = MOID (NEXT (p));
	}
      else if (IS (p, EMPTY_SYMBOL))
       {
	 MOID (p) = M_VOID;
       }
    }
}

/* Collect modes from the syntax tree.  */

static void
get_modes_from_tree (NODE_T *p, int attribute)
{
  for (NODE_T *q = p; q != NO_NODE; FORWARD (q))
    {
      if (IS (q, VOID_SYMBOL))
	MOID (q) = M_VOID;
      else if (IS (q, DECLARER))
	{
	  if (attribute == VARIABLE_DECLARATION)
	    {
	      MOID_T *new_one = get_mode_from_declarer (q);
	      MOID (q) = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, new_one, NO_PACK);
	    }
	  else
	    MOID (q) = get_mode_from_declarer (q);
	}
      else if (IS (q, ROUTINE_TEXT))
	{
	  MOID (q) = get_mode_from_routine_text (SUB (q));
	}
      else if (IS (q, OPERATOR_PLAN))
	{
	  MOID (q) = get_mode_from_operator (SUB (q));
	}
      else if (a68_is_one_of (q, LOC_SYMBOL, HEAP_SYMBOL, STOP))
	{
	  if (attribute == GENERATOR)
	    {
	      MOID_T *new_one = get_mode_from_declarer (NEXT (q));
	      MOID (NEXT (q)) = new_one;
	      MOID (q) = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, NO_NODE, new_one, NO_PACK);
	    }
	}
      else
	{
	  if (attribute == DENOTATION)
	    get_mode_from_denotation (q, 0);
	}
    }

  if (attribute != DENOTATION)
    {
      for (NODE_T *q = p; q != NO_NODE; FORWARD (q))
	{
	  if (SUB (q) != NO_NODE)
	    get_modes_from_tree (SUB (q), ATTRIBUTE (q));
	}
    }
}

//! @brief Collect modes from proc variables.

static void
get_mode_from_proc_variables (NODE_T *p)
{
  if (p != NO_NODE)
    {
      if (IS (p, PROCEDURE_VARIABLE_DECLARATION))
	{
	  get_mode_from_proc_variables (SUB (p));
	  get_mode_from_proc_variables (NEXT (p));
	}
      else if (IS (p, PUBLIC_SYMBOL) || IS (p, QUALIFIER) || IS (p, PROC_SYMBOL) || IS (p, COMMA_SYMBOL))
	{
	  get_mode_from_proc_variables (NEXT (p));
	}
      else if (IS (p, DEFINING_IDENTIFIER))
	{
	  MOID_T *new_one = MOID (NEXT_NEXT (p));
	  MOID (p) = a68_add_mode (&TOP_MOID (&A68_JOB), REF_SYMBOL, 0, p, new_one, NO_PACK);
	}
    }
}

/* Collect modes from proc variable declarations.  */

static void
get_mode_from_proc_var_declarations_tree (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      get_mode_from_proc_var_declarations_tree (SUB (p));

      if (IS (p, PROCEDURE_VARIABLE_DECLARATION))
	get_mode_from_proc_variables (p);
    }
}

/*
 * Various routines to test modes.
 */

/* Whether a mode declaration refers to self or relates to void.
   This uses Lindsey's ying-yang algorithm.  */

static bool
is_well_formed (MOID_T *def, MOID_T *z, bool yin, bool yang, bool video)
{
  if (z == NO_MOID)
    return false;
  else if (yin && yang)
    return z == M_VOID ? video : true;
  else if (z == M_VOID)
    return video;
  else if (IS (z, STANDARD))
    return true;
  else if (IS (z, INDICANT))
    {
      if (def == NO_MOID)
	{
	  /* Check an applied indicant for relation to VOID.  */
	  while (z != NO_MOID)
	    z = EQUIVALENT (z);
	  if (z == M_VOID)
	    return video;
	  else
	    return true;
	}
      else
	{
	  if (z == def || USE (z))
	    return yin && yang;
	  else
	    {
	      USE (z) = true;
	      bool wwf = is_well_formed (def, EQUIVALENT (z), yin, yang, video);
	      USE (z) = false;
	  return wwf;
	    }
	}
    }
  else if (IS_REF (z))
    return is_well_formed (def, SUB (z), true, yang, false);
  else if (IS (z, PROC_SYMBOL))
    return PACK (z) != NO_PACK ? true : is_well_formed (def, SUB (z), true, yang, true);
  else if (IS_ROW (z))
    return is_well_formed (def, SUB (z), yin, yang, false);
  else if (IS_FLEX (z))
    return is_well_formed (def, SUB (z), yin, yang, false);
  else if (IS (z, STRUCT_SYMBOL))
    {
      for (PACK_T *s = PACK (z); s != NO_PACK; FORWARD (s))
	{
	  if (!is_well_formed (def, MOID (s), yin, true, false))
	    return false;
	}
      return true;
    }
  else if (IS (z, UNION_SYMBOL))
    {
      for (PACK_T *s = PACK (z); s != NO_PACK; FORWARD (s))
	{
	  if (!is_well_formed (def, MOID (s), yin, yang, true))
	    return false;
	}
      return true;
    }
  else
    {
      return false;
    }
}

/* Replace a mode by its equivalent mode (walk chain).  */

static void
resolve_eq_members (MOID_T *q)
{
  resolve_equivalent (&SUB (q));
  resolve_equivalent (&DEFLEXED (q));
  resolve_equivalent (&MULTIPLE (q));
  resolve_equivalent (&NAME (q));
  resolve_equivalent (&SLICE (q));
  resolve_equivalent (&TRIM (q));
  resolve_equivalent (&ROWED (q));
  for (PACK_T *p = PACK (q); p != NO_PACK; FORWARD (p))
    resolve_equivalent (&MOID (p));
}

/* Track equivalent tags.  */

static void
resolve_eq_tags (TAG_T *z)
{
  for (; z != NO_TAG; FORWARD (z))
    {
      if (MOID (z) != NO_MOID)
	resolve_equivalent (&MOID (z));
    }
}

/* Bind modes in syntax tree.  */

static void
bind_modes (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      resolve_equivalent (&MOID (p));

      if (SUB (p) != NO_NODE && a68_is_new_lexical_level (p))
	{
	  TABLE_T *s = TABLE (SUB (p));
	  for (TAG_T *z = INDICANTS (s); z != NO_TAG; FORWARD (z))
	    {
	      if (NODE (z) != NO_NODE)
		{
		  resolve_equivalent (&MOID (NEXT_NEXT (NODE (z))));
		  MOID (z) = MOID (NEXT_NEXT (NODE (z)));
		  MOID (NODE (z)) = MOID (z);
		}
	    }
	}
      bind_modes (SUB (p));
    }
}

/* Routines for calculating subordinates for selections, for instance selection
   from REF STRUCT (A) yields REF A fields and selection from [] STRUCT (A)
   yields [] A fields.  */

/* Make name pack.
   Given a pack with modes: M1, M2, ...
   Build a pack with modes: REF M1, REF M2, ...  */

static void
make_name_pack (PACK_T *src, PACK_T **dst, MOID_T **p)
{
  if (src != NO_PACK)
    {
      make_name_pack (NEXT (src), dst, p);
      MOID_T *z = a68_add_mode (p, REF_SYMBOL, 0, NO_NODE, MOID (src), NO_PACK);
      (void) a68_add_mode_to_pack (dst, z, TEXT (src), NODE (src));
    }
}

/* Make flex multiple row pack.
   Given a pack with modes: M1, M2, ...
   Build a pack with modes: []M1, []M2, ...  */

static void
make_flex_multiple_row_pack (PACK_T *src, PACK_T **dst, MOID_T **p, int dim)
{
  if (src != NO_PACK)
    {
      make_flex_multiple_row_pack (NEXT (src), dst, p, dim);
      MOID_T *z = add_row (p, dim, MOID (src), NO_NODE, false);
      z = a68_add_mode (p, FLEX_SYMBOL, 0, NO_NODE, z, NO_PACK);
      (void) a68_add_mode_to_pack (dst, z, TEXT (src), NODE (src));
    }
}

/* Make name struct.  */

static MOID_T *
make_name_struct (MOID_T *m, MOID_T **p)
{
  PACK_T *u = NO_PACK;
  make_name_pack (PACK (m), &u, p);
  return a68_add_mode (p, STRUCT_SYMBOL, DIM (m), NO_NODE, NO_MOID, u);
}

/* Make name row.  */

static MOID_T *
make_name_row (MOID_T *m, MOID_T **p)
{
  if (SLICE (m) != NO_MOID)
    return a68_add_mode (p, REF_SYMBOL, 0, NO_NODE, SLICE (m), NO_PACK);
  else if (SUB (m) != NO_MOID)
    return a68_add_mode (p, REF_SYMBOL, 0, NO_NODE, SUB (m), NO_PACK);
  else
    /* weird, FLEX INT or so ...  */
    return NO_MOID;
}

/* Make multiple row pack.  */

static void
make_multiple_row_pack (PACK_T *src, PACK_T **dst, MOID_T **p, int dim)
{
  if (src != NO_PACK)
    {
      make_multiple_row_pack (NEXT (src), dst, p, dim);
      (void) a68_add_mode_to_pack (dst, add_row (p, dim, MOID (src), NO_NODE, false),
				   TEXT (src), NODE (src));
    }
}

/* Make flex multiple struct.  */

static MOID_T *
make_flex_multiple_struct (MOID_T *m, MOID_T **p, int dim)
{
  PACK_T *u = NO_PACK;
  make_flex_multiple_row_pack (PACK (m), &u, p, dim);
  return a68_add_mode (p, STRUCT_SYMBOL, DIM (m), NO_NODE, NO_MOID, u);
}

/* Make multiple struct.  */

static MOID_T *
make_multiple_struct (MOID_T *m, MOID_T **p, int dim)
{
  PACK_T *u = NO_PACK;
  make_multiple_row_pack (PACK (m), &u, p, dim);
  return a68_add_mode (p, STRUCT_SYMBOL, DIM (m), NO_NODE, NO_MOID, u);
}

/* Whether mode has row.  */

static bool
is_mode_has_row (MOID_T *m)
{
  if (IS (m, STRUCT_SYMBOL) || IS (m, UNION_SYMBOL))
    {
      bool k = false;

      for (PACK_T *p = PACK (m); p != NO_PACK && k == false; FORWARD (p))
	{
	  HAS_ROWS (MOID (p)) = is_mode_has_row (MOID (p));
	  k |= (HAS_ROWS (MOID (p)));
	}
      return k;
    }
  else
    return (HAS_ROWS (m) || IS_ROW (m) || IS_FLEX (m));
}

/* Compute derived modes.  */

static void
compute_derived_modes (MODULE_T *mod)
{
  MOID_T *z;
  int len = 0, nlen = 1;

  /* UNION things.  */
  absorb_unions (TOP_MOID (mod));
  contract_unions (TOP_MOID (mod));
  /* The for-statement below prevents an endless loop.  */
  for (int k = 1; k <= 10 && len != nlen; k++)
    {
      /* Make deflexed modes.  */
      for (z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
	{
	  if (SUB (z) != NO_MOID)
	    {
	      if (IS_REF_FLEX (z) && DEFLEXED (SUB_SUB (z)) != NO_MOID)
		DEFLEXED (z) = a68_add_mode (&TOP_MOID (mod), REF_SYMBOL, 0, NODE (z),
					     DEFLEXED (SUB_SUB (z)), NO_PACK);
	      else if (IS_REF (z) && DEFLEXED (SUB (z)) != NO_MOID)
		DEFLEXED (z) = a68_add_mode (&TOP_MOID (mod), REF_SYMBOL, 0, NODE (z),
					     DEFLEXED (SUB (z)), NO_PACK);
	      else if (IS_ROW (z) && DEFLEXED (SUB (z)) != NO_MOID)
		DEFLEXED (z) = a68_add_mode (&TOP_MOID (mod), ROW_SYMBOL, DIM (z), NODE (z),
					     DEFLEXED (SUB (z)), NO_PACK);
	      else if (IS_FLEX (z) && DEFLEXED (SUB (z)) != NO_MOID)
		DEFLEXED (z) = DEFLEXED (SUB (z));
	      else if (IS_FLEX (z))
		DEFLEXED (z) = SUB (z);
	      else
		DEFLEXED (z) = z;
	    }
	}

      /* Derived modes for stowed modes.  */
      for (z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
	{
	  if (NAME (z) == NO_MOID && IS_REF (z))
	    {
	      if (IS (SUB (z), STRUCT_SYMBOL))
		NAME (z) = make_name_struct (SUB (z), &TOP_MOID (mod));
	      else if (IS_ROW (SUB (z)))
		NAME (z) = make_name_row (SUB (z), &TOP_MOID (mod));
	      else if (IS_FLEX (SUB (z)) && SUB_SUB (z) != NO_MOID)
		NAME (z) = make_name_row (SUB_SUB (z), &TOP_MOID (mod));
	    }

	  if (MULTIPLE (z) != NO_MOID)
	    ;
	  else if (IS_REF (z))
	    {
	      if (MULTIPLE (SUB (z)) != NO_MOID)
		MULTIPLE (z) = make_name_struct (MULTIPLE (SUB (z)), &TOP_MOID (mod));
	    }
	  else if (IS_ROW (z))
	    {
	      if (IS (SUB (z), STRUCT_SYMBOL))
		MULTIPLE (z) = make_multiple_struct (SUB (z), &TOP_MOID (mod), DIM (z));
	    }
	}

      for (z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
	{
	  if (TRIM (z) == NO_MOID && IS_FLEX (z))
	    TRIM (z) = SUB (z);
	  if (TRIM (z) == NO_MOID && IS_REF_FLEX (z))
	    TRIM (z) = a68_add_mode (&TOP_MOID (mod), REF_SYMBOL, 0, NODE (z), SUB_SUB (z), NO_PACK);
	}

      /* Fill out stuff for rows, f.i. inverse relations.  */
      for (z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
	{
	  if (IS_ROW (z) && DIM (z) > 0 && SUB (z) != NO_MOID && !DERIVATE (z))
	    (void) add_row (&TOP_MOID (mod), DIM (z) + 1, SUB (z), NODE (z), true);
	  else if (IS_REF (z) && IS (SUB (z), ROW_SYMBOL) && !DERIVATE (SUB (z)))
	    {
	      MOID_T *x = add_row (&TOP_MOID (mod), DIM (SUB (z)) + 1, SUB_SUB (z), NODE (SUB (z)), true);
	      MOID_T *y = a68_add_mode (&TOP_MOID (mod), REF_SYMBOL, 0, NODE (z), x, NO_PACK);
	      NAME (y) = z;
	    }
	}

      for (z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
	{
	  if (IS_ROW (z) && SLICE (z) != NO_MOID)
	    ROWED (SLICE (z)) = z;
	  if (IS_REF (z))
	    {
	      MOID_T *y = SUB (z);
	      if (SLICE (y) != NO_MOID && IS_ROW (SLICE (y)) && NAME (z) != NO_MOID)
		ROWED (NAME (z)) = z;
	    }
	}

      bind_modes (TOP_NODE (mod));
      for (z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
	{
	  if (IS (z, INDICANT) && NODE (z) != NO_NODE)
	    EQUIVALENT (z) = MOID (NODE (z));
	}
      for (z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
	resolve_eq_members (z);
      resolve_eq_tags (INDICANTS (A68_STANDENV));
      resolve_eq_tags (IDENTIFIERS (A68_STANDENV));
      resolve_eq_tags (OPERATORS (A68_STANDENV));
      resolve_equivalent (&M_STRING);
      resolve_equivalent (&M_COMPLEX);
      resolve_equivalent (&M_LONG_COMPLEX);
      resolve_equivalent (&M_LONG_LONG_COMPLEX);
      resolve_equivalent (&M_SEMA);
      /* UNION members could be resolved.  */
      absorb_unions (TOP_MOID (mod));
      contract_unions (TOP_MOID (mod));
      /* FLEX INDICANT could be resolved.  */
      for (z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
	{
	  if (IS_FLEX (z) && SUB (z) != NO_MOID)
	    {
	      if (SUB_SUB (z) != NO_MOID && IS (SUB_SUB (z), STRUCT_SYMBOL))
		MULTIPLE (z) = make_flex_multiple_struct (SUB_SUB (z), &TOP_MOID (mod), DIM (SUB (z)));
	    }
	}
      /* See what new known modes we have generated by resolving..  */
      for (z = TOP_MOID (mod); z != STANDENV_MOID (&A68_JOB); FORWARD (z))
	{
	  MOID_T *v;

	  for (v = NEXT (z); v != NO_MOID; FORWARD (v))
	    {
	      if (a68_prove_moid_equivalence (z, v))
		{
		  EQUIVALENT (z) = v;
		  EQUIVALENT (v) = NO_MOID;
		}
	    }
	}

      /* Count the modes to check self consistency.  */
      len = nlen;
      for (nlen = 0, z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
	nlen++;
    }

  gcc_assert (M_STRING == M_FLEX_ROW_CHAR);

  /* Find out what modes contain rows.  */
  for (z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
    HAS_ROWS (z) = is_mode_has_row (z);

  /* Check flexible modes.  */
  for (z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
    {
      if (IS_FLEX (z) && !IS (SUB (z), ROW_SYMBOL))
	a68_error (NODE (z), "M does not specify a well formed mode", z);
    }

  /* Check on fields in structured modes f.i. STRUCT (REAL x, INT n, REAL x) is
     wrong.  */
  for (z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
    {
      if (IS (z, STRUCT_SYMBOL) && EQUIVALENT (z) == NO_MOID)
	{
	  PACK_T *s = PACK (z);

	  for (; s != NO_PACK; FORWARD (s))
	    {
	      bool x = true;

	      for (PACK_T *t = NEXT (s); t != NO_PACK && x; FORWARD (t))
		{
		  if (TEXT (s) == TEXT (t))
		    {
		      a68_error (NODE (z), "multiple declaration of field S");
		      while (NEXT (s) != NO_PACK && TEXT (NEXT (s)) == TEXT (t))
			FORWARD (s);
		      x = false;
		    }
		}
	    }
	}
    }

  /* Various union test.  */
  for (z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
    {
      if (IS (z, UNION_SYMBOL) && EQUIVALENT (z) == NO_MOID)
	{
	  PACK_T *s = PACK (z);
	  /* Discard unions with one member.  */
	  if (a68_count_pack_members (s) == 1)
	    a68_error (NODE (z), "M must have at least two components", z);
	  /* Discard incestuous unions with firmly related modes.  */
	  for (; s != NO_PACK; FORWARD (s))
	    {
	      PACK_T *t;

	      for (t = NEXT (s); t != NO_PACK; FORWARD (t))
		{
		  if (MOID (t) != MOID (s))
		    {
		      if (a68_is_firm (MOID (s), MOID (t)))
			a68_error (NODE (z), "M has firmly related components", z);
		    }
		}
	    }

	  /* Discard incestuous unions with firmly related subsets.  */
	  for (s = PACK (z); s != NO_PACK; FORWARD (s))
	    {
	      MOID_T *n = a68_depref_completely (MOID (s));

	      if (IS (n, UNION_SYMBOL) && a68_is_subset (n, z, NO_DEFLEXING))
		  a68_error (NODE (z), "M has firmly related subset M", z, n);
	    }
	}
    }

  /* Wrap up and exit.  */
  a68_free_postulate_list (A68 (top_postulate), NO_POSTULATE);
  A68 (top_postulate) = NO_POSTULATE;
}

/* Make list of all modes in the program.  */

void
a68_make_moid_list (MODULE_T *mod)
{
  bool cont = true;

  /* Collect modes from the syntax tree.  */
  reset_moid_tree (TOP_NODE (mod));
  get_modes_from_tree (TOP_NODE (mod), STOP);
  get_mode_from_proc_var_declarations_tree (TOP_NODE (mod));

  /* Connect indicants to their declarers.  */
  for (MOID_T *z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
    {
      if (IS (z, INDICANT))
	{
	  NODE_T *u = NODE (z);
	  gcc_assert (NEXT (u) != NO_NODE);
	  gcc_assert (NEXT_NEXT (u) != NO_NODE);
	  gcc_assert (MOID (NEXT_NEXT (u)) != NO_MOID);
	  EQUIVALENT (z) = MOID (NEXT_NEXT (u));
	}
    }

  /* Checks on wrong declarations.  */
  for (MOID_T *z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
    USE (z) = false;

  for (MOID_T *z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
    {
      if (IS (z, INDICANT) && EQUIVALENT (z) != NO_MOID)
	{
	  if (!is_well_formed (z, EQUIVALENT (z), false, false, true))
	    {
	      a68_error (NODE (z), "M does not specify a well formed mode", z);
	      cont = false;
	    }
	}
    }

  for (MOID_T *z = TOP_MOID (mod); cont && z != NO_MOID; FORWARD (z))
    {
      if (IS (z, INDICANT) && EQUIVALENT (z) != NO_MOID)
	;
      else if (NODE (z) != NO_NODE)
	{
	  if (!is_well_formed (NO_MOID, z, false, false, true))
	    a68_error (NODE (z), "M does not specify a well formed mode", z);
	}
    }

  for (MOID_T *z = TOP_MOID (mod); z != NO_MOID; FORWARD (z))
    {
      if (USE (z))
	gcc_unreachable ();
    }

  if (ERROR_COUNT (mod) != 0)
    return;

  compute_derived_modes (mod);
  a68_init_postulates ();
}
