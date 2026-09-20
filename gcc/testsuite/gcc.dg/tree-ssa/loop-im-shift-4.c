/* { dg-do run { target bitint } } */
/* { dg-options "-O2 -std=c23 -fdump-tree-optimized" } */

/* A non-power-of-two precision clamps the count to the precision - 1.  */

__attribute__ ((noipa)) void
f (int *p, int n, _BitInt(24) a, unsigned int s, int c, _BitInt(24) *q)
{
  for (int i = 0; i < n; i++)
    if (p[i] > c)
      q[i] = ((a << s) * 5) / 7;
}

int
main (void)
{
  int p[2] = { 1, 9 };
  _BitInt(24) q[2] = { 0, 0 };
  f (p, 2, 1, 40u, 100, q);
  if (q[0] != 0 || q[1] != 0)
    __builtin_abort ();
  f (p, 2, 1, 8u, 4, q);
  if (q[0] != 0 || q[1] != ((_BitInt(24)) 1 << 8) * 5 / 7)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "MIN_EXPR <\[^,\]*, 23>" "optimized" } } */
