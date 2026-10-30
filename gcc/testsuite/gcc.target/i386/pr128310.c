/* PR target/128310 */
/* { dg-do run } */
/* { dg-options "-O2 -mcrc32" } */
/* { dg-require-effective-target sse4 } */

#include "sse4_2-check.h"
#include <immintrin.h>
#include <stdint.h>

__attribute__((noipa)) int bh (uint32_t c)
{ return _mm_crc32_u8 (c, 0x7f) == _mm_crc32_u16 (c, 0x7f); }

__attribute__((noipa)) int bs (uint32_t c)
{ return _mm_crc32_u8 (c, 0x7f) == _mm_crc32_u32 (c, 0x7f); }

__attribute__((noipa)) int hs (uint32_t c)
{ return _mm_crc32_u16 (c, 0x7f) == _mm_crc32_u32 (c, 0x7f); }

#ifdef __x86_64__
__attribute__((noipa)) int bq (uint32_t c)
{ return _mm_crc32_u8 (c, 0x7f) == _mm_crc32_u64 (c, 0x7f); }
#endif

static void
sse4_2_test (void)
{
  if (bh (7) || bs (7) || hs (7))
    __builtin_abort ();
#ifdef __x86_64__
  if (bq (7))
    __builtin_abort ();
#endif
}
