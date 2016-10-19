/* PR tree-optimization/61839.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */
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


/* Dont optimize 972195717 / 0 in function foo.  */
/* { dg-final { scan-tree-dump-times "972195717 / " 1  "evrp" } } */
/* Dont optimize 972195717 % 0 in function bar.  */
/* { dg-final { scan-tree-dump-times "972195717 % " 1 "evrp" } } */
/* May optimize in function bar2, but EVRP doesn't perform this yet.  */
/* { dg-final { scan-tree-dump-times "972195715 % " 0 "evrp" { xfail *-*-* } } } */
