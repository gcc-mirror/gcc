/* { dg-do compile } */
/* Specify power9 to ensure the vectorization is profitable
   and test point stands, otherwise it could be not profitable
   to vectorize.  */
/* { dg-additional-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target powerpc_vsx } */

/* Verify we cost the exact count for required vec_perm.  */

int x[1024], y[1024];

void
foo ()
{
  for (int i = 0; i < 512; ++i)
    {
      x[2 * i] = y[1023 - (2 * i)];
      x[2 * i + 1] = y[1023 - (2 * i + 1)];
    }
}

/* { dg-final { scan-tree-dump-times "2 times vec_perm" 1 "vect" } } */
