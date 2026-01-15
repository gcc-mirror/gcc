/* Test that vectorization assigns multiplicity and copyid discriminators.
   { dg-do compile }
   { dg-options "-O3 -g -ftree-vectorize -fno-vect-cost-model" }
   { dg-require-effective-target vect_int }
    */

void
test_vectorize (int *a, int *b, int *c, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      a[i] = b[i] + c[i];  /* Main vectorized loop and epilog */
    }
}

/* { dg-final { scan-assembler "\\.loc 1 13 \[0-9\]+ is_stmt 0 discriminator \[0-9\]+" } } */
