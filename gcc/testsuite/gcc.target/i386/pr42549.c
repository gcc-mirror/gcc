/* { dg-do run } */
/* { dg-options "-O2 -m3dnow" } */

#include "mmx-3dnow-check.h"

#include <mm3dnow.h>

typedef union {
  float f[2];
  __m64 v;
} vec_t;

void __attribute__ ((noinline))
Butterfly_3 (__m64 * D, __m64 SC)
{
  __m64 T, T1;

  T = _m_pfmul (D[1], SC);
  T1 = D[0];
  D[0] = _m_pfadd (T1, T);
  D[1] = _m_pfsub (T1, T);
}

static void
mmx_3dnow_test (void)
{
  vec_t D[2] = { { .f = { 2.0f, 3.0f } },
		 { .f = { 4.0f, 5.0f } } };

  const vec_t SC = { .f = { 1.0f, 1.0f } };

  Butterfly_3 (&D[0].v, SC.v);
  _m_femms ();

  if (D[1].f[0] != -2.0f || D[1].f[1] != -2.0f)
    abort ();
}
