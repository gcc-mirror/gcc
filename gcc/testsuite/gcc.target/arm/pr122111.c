/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef long long wide;

void g(void);
wide f(int *a, unsigned t)
{
  wide i = t;
  i *= 4;
  unsigned long ai = (__SIZE_TYPE__)a;
  return i + ai;
}
