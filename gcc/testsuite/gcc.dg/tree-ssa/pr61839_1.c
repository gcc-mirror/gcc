/* PR tree-optimization/61839.  */
/* { dg-do run } */
/* { dg-options "-O2 -fdisable-tree-evrp -fdisable-tree-ethread -fdisable-tree-threadfull1 -fdump-tree-vrp1 -fdump-tree-optimized" } */
/* { dg-require-effective-target int32plus } */

__attribute__ ((noinline))
int foo ()
{
  int a = -1;
  volatile unsigned b = 1U;
  int c = 1;
  c = (a + 972195718) >> (1LU <= b);
  if (c == 486097858)
    ;
  else
    __builtin_abort ();
  return 0;
}

__attribute__ ((noinline))
int bar ()
{
  int a = -1;
  volatile unsigned b = 1U;
  int c = 1;
  c = (a + 972195718) >> (b ? 2 : 3);
  if (c == 243048929)
    ;
  else
    __builtin_abort ();
  return 0;
}

int main ()
{
  foo ();
  bar ();
}

/* Scan for c = 972195717) >> [0, 1] in function foo.  */
/* { dg-final { scan-tree-dump-times "972195717 : 486097858" 1  "vrp1" } } */

/* Previously we were checking for two ?: with constant PHI arguments,
   but now we collapse them into one.  */
/* Scan for c = 972195717) >> [2, 3] in function bar.  */
/* { dg-final { scan-tree-dump-times "243048929 : 121524464" 1  "vrp1" } } */

/* { dg-final { scan-tree-dump-times "486097858" 0  "optimized" } } */
