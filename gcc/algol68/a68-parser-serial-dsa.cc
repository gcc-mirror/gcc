/* Check dynamic stack usage in serial clauses.
   Copyright (C) 2025 Jose E. Marchesi.

   Written by Jose E. Marchesi.

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

/* This file implements a phase that determines what serial clauses contain
   phrases whose elaboration may involve dynamic stack allocation.  It
   annotates the SERIAL_CLAUSE parse nodes by setting the DYNAMIC_STACK_ALLOCS
   flag.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"

#include "a68.h"

/* Uncomment the following line for debugging traces.  */
/* #define SERIAL_DSA_DEBUG */

static void
serial_dsa_check_serial_clause (NODE_T *p, bool *dsa)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, GENERATOR))
	{
	  /* LOC generators always result in dyamic stack allocation regardless
	     of the mode of the allocated value.  */
	  if (IS (SUB (p), LOC_SYMBOL))
	    {
#ifdef SERIAL_DSA_DEBUG
	      fprintf (stderr, "serial_dsa: %s:%d: loc generator implies DSA\n",
		       FILENAME (LINE (INFO (p))),
		       LINE_NUMBER (p));
#endif
	      *dsa = true;
	      return;
	    }
	}
      else if (IS (p, DEFINING_IDENTIFIER))
	{
	  /* Variable declarations of values with sample loc generators will
	     result in dynamic stack allocation.

	     Note that label declarations do no have a mode, so we have to
	     check for MOID (p).  */

	  if (MOID (p) != NO_MOID && IS_REF (MOID (p)))
	    {
	      bool heap = HEAP (TAX (p)) == HEAP_SYMBOL;
	      if (HAS_ROWS (SUB (MOID (p))) && !heap)
		{
#ifdef SERIAL_DSA_DEBUG		  
		  fprintf (stderr,
			   "serial_dsa: %s:%d: defining identifier %s implies DSA\n",
			   FILENAME (LINE (INFO (p))),
			   LINE_NUMBER (p),
			   NSYMBOL (p));
#endif
		  *dsa = true;
		  return;
		}
	    }
	}
      else
	{
	  /* Inner serial clauses will take care of their own.  Code in routine
	     texts will not impact the stack of the containing serial
	     clause.  */
	  if (!IS (p, SERIAL_CLAUSE) && !IS (p, ROUTINE_TEXT))
	    serial_dsa_check_serial_clause (SUB (p), dsa);
	}
    }
}

void
a68_serial_dsa (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      a68_serial_dsa (SUB (p));
      if (IS (p, SERIAL_CLAUSE))
	{
	  bool dsa = false;
	  serial_dsa_check_serial_clause (SUB (p), &dsa);
	  DYNAMIC_STACK_ALLOCS (p) = dsa;
#ifdef SERIAL_DSA_DEBUG
	  if (dsa)
	    {
	      fprintf (stderr, "serial_dsa: %s:%d: marking serial clause %p as DSA\n",
		       FILENAME (LINE (INFO (p))),
		       LINE_NUMBER (p),
		       (void *) p);
	    }
	  
#endif
	}
    }
}
