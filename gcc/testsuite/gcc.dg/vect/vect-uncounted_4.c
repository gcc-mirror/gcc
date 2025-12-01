/* Check vectorization of uncounted reductions.  */
/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

int
foo (int *a0, int *aN, int accum)
{
  int i = 0;
  while (1)
    {
      if (a0[i++] == *aN)
	return accum;
      accum += a0[i];
     }
}

/* { dg-final { scan-tree-dump "Loop being analyzed as uncounted." "vect" } } */
/* { dg-final { scan-tree-dump "Detected reduction." "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
/* { dg-final { scan-tree-dump {vect_accum_([0-9_.]+) = PHI <vect_accum_[0-9_.]+\([0-9]+\), { (0, )+0 }\([0-9]+\)>.*# vect_accum_[0-9_.]+ = PHI <vect_accum_\1\([0-9]+\)>} "vect" } } */
