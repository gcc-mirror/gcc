/* PR tree-optimization/61839.  */
/* { dg-do compile } */
/* Disable jump threading, we want to avoid separating the division/modulo
   by zero paths - we'd isolate those only later.  */
/* { dg-options "-O2 -fno-thread-jumps -fdump-tree-evrp" } */
/* { dg-require-effective-target int32plus } */

__attribute__ ((noinline))
int foo ()
{
  int a = -1;
  volatile unsigned b = 1U;
  int c = 1;
  c = (a + 972195718) / (b ? 2 : 0);
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
  c = (a + 972195718) % (b ? 2 : 0);
  if (c == 1)
    ;
  else
    __builtin_abort ();
  return 0;
}

__attribute__ ((noinline))
int bar2 ()
{
  int a = -1;
  volatile unsigned b = 1U;
  int c = 1;
  c = (a + 972195716) % (b ? 1 : 2);
  if (c == 0)
    ;
  else
    __builtin_abort ();
  return 0;
}

/* Ensure we are folding modulus sub-ranges properly.  */
__attribute__ ((noinline))
int mod (int a, int b)
{
  int v1, v2;
  v1 = (a < 10) ? 12 : 24;
  v2 = (b > 20) ? 3 : 6;

  if (a > 20)
    v1 = v1 * 2;
  if (b > 20)
    v2 = v2 * 2;

  if (a == b)
    v2 = 0;
    
  /* v1 == 12, 24, or 48.  v2 == 0, 3, 6, or 12. */
  int c = v1 % v2;
  if (c == 0)
    ;
  else
    __builtin_abort ();
  return 0;
}

/* EVRP now makes transformations in all functions, leaving a single
 * builtin_abort call in bar2. */
/* { dg-final { scan-tree-dump-times "__builtin_abort" 1 "evrp" } } */

/* Make sure to optimize 972195717 / 0 in function foo.  */
/* { dg-final { scan-tree-dump-times "972195717 / " 0  "evrp" } } */
/* Make sure  to optimize 972195717 % 0 in function bar.  */
/* { dg-final { scan-tree-dump-times "972195717 % " 0 "evrp" } } */
/* Make sure to optimize 972195717 % [1,2] function bar2.  */
/* { dg-final { scan-tree-dump-times "972195715 % " 0 "evrp" } } */
/* [12,12][24,24][48,48] % [0,0][3,3][6,6][12,12] == [0,0] */
/* { dg-final { scan-tree-dump-times "%" 0 "evrp" } } */
