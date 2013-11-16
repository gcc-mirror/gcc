/* This file contains routines to construct and validate Cilk Plus
   constructs within the C and C++ front ends.

   Copyright (C) 2013  Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "c-common.h"

/* Validate the body of a _Cilk_for construct or a <#pragma simd> for
   loop.

   Returns true if there were no errors, false otherwise.  */

bool
c_check_cilk_loop (location_t loc, tree decl)
{
  if (TREE_THIS_VOLATILE (decl))
    {
      error_at (loc, "iteration variable cannot be volatile");
      return false;
    }
  return true;
}

/* Validate and emit code for <#pragma simd> clauses.  */

tree
c_finish_cilk_clauses (tree clauses)
{
  for (tree c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      tree prev = clauses;

      /* If a variable appears in a linear clause it cannot appear in
	 any other OMP clause.  */
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR)
	for (tree c2 = clauses; c2; c2 = OMP_CLAUSE_CHAIN (c2))
	  {
	    if (c == c2)
	      continue;
	    enum omp_clause_code code = OMP_CLAUSE_CODE (c2);

	    switch (code)
	      {
	      case OMP_CLAUSE_LINEAR:
	      case OMP_CLAUSE_PRIVATE:
	      case OMP_CLAUSE_FIRSTPRIVATE:
	      case OMP_CLAUSE_LASTPRIVATE:
	      case OMP_CLAUSE_REDUCTION:
		break;

	      case OMP_CLAUSE_SAFELEN:
		goto next;

	      default:
		gcc_unreachable ();
	      }

	    if (OMP_CLAUSE_DECL (c) == OMP_CLAUSE_DECL (c2))
	      {
		error_at (OMP_CLAUSE_LOCATION (c2),
			  "variable appears in more than one clause");
		inform (OMP_CLAUSE_LOCATION (c),
			"other clause defined here");
		// Remove problematic clauses.
		OMP_CLAUSE_CHAIN (prev) = OMP_CLAUSE_CHAIN (c2);
	      }
	  next:
	    prev = c2;
	  }
    }
  return clauses;
}
