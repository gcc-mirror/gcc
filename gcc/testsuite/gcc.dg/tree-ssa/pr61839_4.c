/* PR tree-optimization/61839.  */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-vrp1 -fdisable-tree-evrp -fdump-tree-optimized" } */
/* { dg-require-effective-target int32plus } */

__attribute__ ((noinline))
int foo (int a, unsigned b)
{
  unsigned c = 1;
  if (b >= 1 && b <= ((unsigned)(-1) - 1))
    return 0;
  c = b >> 4;
  if (c == 268435455)
    ;
  else
    __builtin_abort ();
  return 0;
}

int main ()
{
  volatile unsigned b = (unsigned)(-1);
  foo (-1, b);
}

/* Scan for ~[1, 4294967294] >> 4 in function foo.  */
/* { dg-final { scan-tree-dump-times "0 : 268435455" 1  "vrp1" } } */
/* { dg-final { scan-tree-dump-times "268435455" 0  "optimized" } } */
