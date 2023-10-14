/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-additional-options "-mvsx" } */

/* Verify we do cost the required vec_perm.  */

int
foo (int *a, int *b, int len)
{
  int i;
  int *a1 = a;
  int *a0 = a1 - 4;
  for (i = 0; i < len; i++)
    {
      *b = *a0 + *a1;
      b--;
      a0++;
      a1++;
    }
  return 0;
}

/* The reason why it doesn't check the exact count is that
   we can get more than 1 vec_perm when it's compiled with
   partial vector capability like Power10 (retrying for
   the epilogue) or it's complied without unaligned vector
   memory access support (realign).  */
/* { dg-final { scan-tree-dump {\mvec_perm\M} "vect" } } */
