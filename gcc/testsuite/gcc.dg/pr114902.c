/* PR rtl-optimization/114902 */
/* { dg-do run } */
/* { dg-options "-O1 -fno-tree-fre -fno-tree-forwprop -fno-tree-ccp -fno-tree-dominator-opts" } */

__attribute__((noipa))
int foo (int x)
{
  int a = ~x;
  int t = a & 1;
  int e = -t;
  int b = e >= -1;
  if (b)
    return 0;
  __builtin_trap ();
}

int
main ()
{
  foo (-1);
  foo (0);
  foo (1);
}
