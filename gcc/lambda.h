/* Lambda matrix interface.
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dberlin@dberlin.org>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */
 
#ifndef LAMBDA_H
#define LAMBDA_H

typedef int *lambda_vector;

/* Allocate a new vector of given SIZE.  */

static inline lambda_vector
lambda_vector_new (int size)
{
  return ggc_alloc_cleared (size * sizeof(int));
}

/* Clear out vector VEC1 of length SIZE.  */

static inline void
lambda_vector_clear (lambda_vector vec1, int size)
{
  memset (vec1, 0, size * sizeof (int));
}

/* Print out a vector VEC of length N to OUTFILE.  */

static inline void
print_lambda_vector (FILE * outfile, lambda_vector vector, int n)
{
  int i;

  for (i = 0; i < n; i++)
    fprintf (outfile, "%3d ", vector[i]);
  fprintf (outfile, "\n");
}


#endif /* LAMBDA_H  */

