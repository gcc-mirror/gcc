/* { dg-do compile } */
/* { dg-options "-O2 -g -w" } */

void f ()
{
  int n = 5;
  struct { char a[n]; } x;
  g (x, x);
}
int g (double x, double y)
{
  return __builtin_isgreater (x, y);
}
