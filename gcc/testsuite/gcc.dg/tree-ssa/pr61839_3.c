/* PR tree-optimization/61839.  */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-vrp1 -fdump-tree-optimized" } */

__attribute__ ((noinline))
int foo (int a, unsigned b)
{
  int c = 1;
  b =  a ? 12 : 13;
  c = b << 8;
  if (c == 3072)
    ;
  else
    __builtin_abort ();
  return 0;
}

int main ()
{
  volatile unsigned b = 1U;
  foo (-1, b);
}

/* Scan for c [12, 13] << 8 in function foo.  */
/* { dg-final { scan-tree-dump-times "3072 : 3328" 2  "vrp1" } } */
/* { dg-final { scan-tree-dump-times "3072" 0  "optimized" } } */
