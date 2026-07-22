/* PR rtl-optimization/126184 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-ter -fno-tree-coalesce-vars -fdump-rtl-ce1" } */
/* { dg-additional-options "--param=max-rtl-if-conversion-unpredictable-cost=100" } */

__attribute__ ((noipa)) unsigned long long
f (unsigned long long c, unsigned long long x, unsigned long long y,
   unsigned long long z)
{
  if (c != 0)
    {
      c = x + 1;
      x = c * y;
      c = z + 3;
      y = c * x;
    }

  return x + y;
}

int
main (void)
{
  if (f (1, 13, 17, 23) != 6426)
    __builtin_abort ();
  if (f (0, 13, 17, 23) != 30)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_convert_multiple_sets" 1 "ce1" } } */
