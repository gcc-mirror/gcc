/* PR tree-optimization/61839.  */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-vrp-thread1 -fdisable-tree-evrp -fdump-tree-optimized -fdisable-tree-ethread -fdisable-tree-thread1" } */
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
/* { dg-final { scan-tree-dump-times "486097858 : 972195717" 1  "vrp-thread1" } } */

/* Previously we were checking for two ?: with constant PHI arguments,
   but now we collapse them into one.  */
/* Scan for c = 972195717) >> [2, 3] in function bar.  */
/* { dg-final { scan-tree-dump-times "243048929 : 121524464" 1  "vrp-thread1" } } */

/* { dg-final { scan-tree-dump-times "486097858" 0  "optimized" } } */
