/* PR target/99754 */
/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"
#include <emmintrin.h>

static void
sse2_test (void)
{
  union { unsigned char buf[32]; long long ll; } u;
  u.buf[1] = 0xfe;
  u.buf[2] = 0xca;
  u.buf[17] = 0xaa;
  u.buf[18] = 0x55;
  _mm_storeu_si16 (&u.buf[17], _mm_loadu_si16 (&u.buf[1]));
  if (u.buf[17] != 0xfe || u.buf[18] != 0xca)
    abort ();
}
