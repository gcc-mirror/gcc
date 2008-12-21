/* { dg-options "-O2 -mr10k-cache-barrier=store -mips4 -mbranch-likely -mno-abicalls" } */
int bar (int);

/* Test that code after a branch-likely does not get an unnecessary
   cache barrier.  */

NOMIPS16 void
foo (int n, int *x)
{
  do
    n = bar (n * 4 + 1);
  while (n);
  /* The preceding branch should be a branch likely, with the shift as
     its delay slot.  We therefore don't need a cache barrier here.  */
  x[0] = 0;
}

/* { dg-final { scan-assembler-not "\tcache\t" } } */
