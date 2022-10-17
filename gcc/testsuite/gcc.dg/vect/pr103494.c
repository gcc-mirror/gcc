/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

typedef int T1;
typedef signed char T2;

T1
f (T1 *d, T2 *x, int n)
{
  unsigned char res = 0;
  for (int i = 0; i < n; ++i)
    res += d[x[i]];
  return res;
}
