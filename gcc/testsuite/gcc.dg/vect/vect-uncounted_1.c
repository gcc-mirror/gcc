/* Check we can derive scalar IV return value from vectorized IV value,
   resetting it on entry into the scalar epilogue loop via BIT_FIELD_REF.  */
/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

int
foo (int *haystack, int needle)
{
  int i = 0;
  while (1)
    {
      if (haystack[i] == needle)
	return i;
      i++;
     }
}

/* { dg-final { scan-tree-dump "Loop being analyzed as uncounted." "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
/* Ensure we reset our scalar IV so as to repeat the last vector iteration.  */
/* { dg-final { scan-tree-dump {_[0-9_]+ = BIT_FIELD_REF <vect_i_[0-9_.]+, [0-9]+, 0>} "vect" } } */

