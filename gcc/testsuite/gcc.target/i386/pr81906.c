/* PR target/81906 */
/* { dg-do run { target *-*-linux* *-*-gnu* } }
/* { dg-options "-O2 -frounding-math" } */

#include <fenv.h>

int
main ()
{
  #define N 12
  double a[N] = { 2.0, 2.25, 2.5, 2.75, 3.5, -2.0, -2.25, -2.5, -2.75, -3.5, 0x2.0p53, -0x2.0p53 };
  double b[N], c[N], d[N], e[N];
  double be[N] = { 2.0, 2.0, 2.0, 3.0, 4.0, -2.0, -2.0, -2.0, -3.0, -4.0, 0x2.0p53, -0x2.0p53 };
  double ce[N] = { 2.0, 2.0, 2.0, 2.0, 3.0, -2.0, -3.0, -3.0, -3.0, -4.0, 0x2.0p53, -0x2.0p53 };
  double de[N] = { 2.0, 3.0, 3.0, 3.0, 4.0, -2.0, -2.0, -2.0, -2.0, -3.0, 0x2.0p53, -0x2.0p53 };
  double ee[N] = { 2.0, 2.0, 2.0, 2.0, 3.0, -2.0, -2.0, -2.0, -2.0, -3.0, 0x2.0p53, -0x2.0p53 };
  asm volatile ("" : : "g" (a), "g" (be), "g" (ce), "g" (de), "g" (ee) : "memory");

  int i;
  fesetround (FE_TONEAREST);
  for (i = 0; i < N; ++i)
    b[i] = __builtin_rint (a[i]);
  fesetround (FE_DOWNWARD);
  for (i = 0; i < N; ++i)
    c[i] = __builtin_rint (a[i]);
  fesetround (FE_UPWARD);
  for (i = 0; i < N; ++i)
    d[i] = __builtin_rint (a[i]);
  fesetround (FE_TOWARDZERO);
  for (i = 0; i < N; ++i)
    e[i] = __builtin_rint (a[i]);
  fesetround (FE_TONEAREST);
  for (i = 0; i < N; ++i)
    if (b[i] != be[i] || c[i] != ce[i] || d[i] != de[i] || e[i] != ee[i])
      __builtin_abort ();
  return 0;
}
