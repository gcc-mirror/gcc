/* Verify that gimple-level NRV is occurring even for RESULT_DECLs.  */
/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } } */
/* { dg-options "-O -fdump-tree-optimized" } */

struct P
{
  long long l;
  int a;
  unsigned int b;
  P(long long x) : l(x) {}
};

P foo (P);
P bar (P);

P foo (P x)
{
  P y = P (-1LL);
  y = bar (x);
  return y;
}

/* { dg-final { scan-tree-dump-times "return slot optimization" 1 "optimized" } } */
