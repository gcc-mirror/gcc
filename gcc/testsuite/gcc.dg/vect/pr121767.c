/* { dg-do compile } */
/* { dg-additional-options "-mcpu=neoverse-v2" { target aarch64-*-* } } */

int foo (int x, int y, int n)
{
  for (int i = 0; i < n; ++i)
    x = x % y;
  return x;
}
