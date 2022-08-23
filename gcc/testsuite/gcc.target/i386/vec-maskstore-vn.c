/* { dg-do compile } */
/* { dg-options "-O3 -mavx2 -fdump-tree-fre5" } */

void __attribute__((noinline,noclone))
foo (int *out, int *res)
{
  int mask[] = { 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1 };
  int i;
  for (i = 0; i < 16; ++i)
    {
      if (mask[i])
        out[i] = i;
    }
  int o0 = out[0];
  int o7 = out[7];
  int o14 = out[14];
  int o15 = out[15];
  res[0] = o0;
  res[2] = o7;
  res[4] = o14;
  res[6] = o15;
}

/* Vectorization produces .MASK_STORE, unrolling will unroll the two
   vector iterations.  FRE5 after that should be able to CSE
   out[7] and out[15], but leave out[0] and out[14] alone.  */
/* { dg-final { scan-tree-dump " = o0_\[0-9\]+;" "fre5" } } */
/* { dg-final { scan-tree-dump " = 7;" "fre5" } } */
/* { dg-final { scan-tree-dump " = o14_\[0-9\]+;" "fre5" } } */
/* { dg-final { scan-tree-dump " = 15;" "fre5" } } */
