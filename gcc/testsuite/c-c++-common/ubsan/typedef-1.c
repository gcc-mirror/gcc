/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

typedef int V;
int
foo (void)
{
  V v = 9;
  int a = 3;
  v += v % a;
  return v / 3;
}
