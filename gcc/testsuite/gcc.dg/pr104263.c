/* PR tree-optimization/104263 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug -fnon-call-exceptions -fno-inline-small-functions" } */

int n;

int
bar (void)
{
  int a;

  n = 0;
  a = 0;

  return n;
}

__attribute__ ((pure, returns_twice)) int
foo (void)
{
  n = bar () + 1;
  foo ();

  return 0;
}
