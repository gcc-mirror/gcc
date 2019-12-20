/* PR tree-optimization/92930 */
/* { dg-do compile { target untyped_assembly } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "__builtin_apply " "optimized" } } */
/* { dg-final { scan-tree-dump "__builtin_apply_args" "optimized" } } */

void foo (int a, int b, int c, int d, int e, int f, int g);

static void bar (int a, ...)
{
  __builtin_apply (foo, __builtin_apply_args (), 20);
}

int
main ()
{
  bar (1024, 1025, 1026, 1027, 1028, 1029, 1030);
  return 0;
}
