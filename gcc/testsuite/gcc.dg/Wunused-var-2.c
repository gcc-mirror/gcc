/* { dg-do compile } */
/* { dg-options "-Wunused" } */

int
f1 (void)
{
  int a;
  int b;
  int c;
  int d;
  int e;
  a = 1;
  b = 2;
  c = 3;
  d = 4;
  e = 5;
  return sizeof (a) + ((__typeof (b)) 1) + __alignof__ (c)
         + __builtin_choose_expr (1, d, e);
}
