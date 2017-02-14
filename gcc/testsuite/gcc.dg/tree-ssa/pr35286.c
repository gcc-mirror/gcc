/* { dg-do compile } */ 
/* { dg-options "-O2 -fno-code-hoisting -fdump-tree-pre-stats" } */
int g2;
struct A {
    int a; int b;
}g1;
int foo(int a, int b)
{
  if (a > 0)
    {
      g1.a = a+ b;
    }
  else
    g1.a = b;

  g2 = a+b;

  return g1.a;
}
/* We will eliminate the g1.a from the return statement as fully redundant,
   and remove one calculation of a + b. */
/* { dg-final { scan-tree-dump-times "Eliminated: 2" 1 "pre"} } */
