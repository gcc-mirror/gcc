/* { dg-do compile } */
/* { dg-options "-O2 -mlsx -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "__builtin_popcount|\\.POPCOUNT" 1 "optimized" } } */

int
PopCount (long b)
{
  int c = 0;

  while (b)
    {
      b &= b - 1;
      c++;
    }

  return c;
}
