/* { dg-do compile } */

__attribute__ ((pure, returns_twice)) int
foo (int x)
{
  int a;

  a = x ? 3 : 0;
  x /= a;
  a = foo (x);
  if (x == a)
    __builtin_unreachable ();

  return 0;
}
