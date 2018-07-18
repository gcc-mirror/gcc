/* PR rtl-optimization/83985 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mcpu=e300c3 -mtune=e300c3" { target { powerpc*-*-* && ilp32 } } } */

long long int v;

void
foo (int x)
{
  if (x == 0)
    return;

  while (v < 2)
    {
      signed char *a;
      v /= x;
      a = v == 0 ? (signed char *) &x : (signed char *) &v;
      ++*a;
      ++v;
    }

  while (1)
    ;
}
