/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-additional-options "-mvsx" } */

/* Verify we do cost the required vec_perm.  */

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
/* The reason why it doesn't check the exact count is that
   retrying for the epilogue with partial vector capability
   like Power10 can result in more than 1 vec_perm.  */
/* { dg-final { scan-tree-dump {\mvec_perm\M} "vect" } } */
