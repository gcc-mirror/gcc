/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3" }

/* This test case is partially extracted from case
   gcc.dg/vect/vect-avg-16.c, it's to verify we don't
   cost a store with vec_to_scalar when we shouldn't.  */

void
test (signed char *restrict a, signed char *restrict b, signed char *restrict c,
      int n)
{
  for (int j = 0; j < n; ++j)
    {
      for (int i = 0; i < 16; ++i)
	a[i] = (b[i] + c[i]) >> 1;
      a += 20;
      b += 20;
      c += 20;
    }
}

/* { dg-final { scan-tree-dump-times "vec_to_scalar" 0 "vect" } } */
