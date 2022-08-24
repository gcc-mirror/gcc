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
  u.buf[1] = 0xbe;
  u.buf[2] = 0xba;
  u.buf[3] = 0xfe;
  u.buf[4] = 0xca;
  u.buf[17] = 0xaa;
  u.buf[18] = 0x55;
  u.buf[19] = 0xaa;
  u.buf[20] = 0x55;
  _mm_storeu_si32 (&u.buf[17], _mm_loadu_si32 (&u.buf[1]));
  if (u.buf[17] != 0xbe || u.buf[18] != 0xba || u.buf[19] != 0xfe || u.buf[20] != 0xca)
    abort ();
}
