/* { dg-do compile } */
/* { dg-options "-fgimple" } */

int __GIMPLE() f(int x, int y)
{
  int a;
  a = (x < y) ? 1 : 2; /* { dg-error "expected" } */
  return a;
}
