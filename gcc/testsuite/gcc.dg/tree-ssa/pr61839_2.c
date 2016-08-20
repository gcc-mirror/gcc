/* PR tree-optimization/61839.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */
/* { dg-require-effective-target int32plus } */

__attribute__ ((noinline))
int foo ()
{
  int a = -1;
  volatile unsigned b = 1U;
  int c = 1;
  c = (a + 972195718) / (b ? 1 : 0);
  if (c == 972195717)
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
  c = (a + 972195718) % (b ? 1 : 0);
  if (c == 972195717)
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
  if (c == 972195715)
    ;
  else
    __builtin_abort ();
  return 0;
}


/* Dont optimize 972195717 / 0 in function foo.  */
/* { dg-final { scan-tree-dump-times "972195717 / _" 1  "vrp1" } } */
/* Dont optimize 972195717 % 0 in function bar.  */
/* { dg-final { scan-tree-dump-times "972195717 % _" 1 "vrp1" } } */
/* Optimize in function bar2.  */
/* { dg-final { scan-tree-dump-times "972195715 % _" 0 "vrp1" } } */
