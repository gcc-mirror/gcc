/* PR target/101549 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4 -mno-crc32" } */

#include <immintrin.h>

unsigned int
test_mm_crc32_u8 (unsigned int CRC, unsigned char V)
{
  return _mm_crc32_u8 (CRC, V);
}

/* { dg-error "target specific option mismatch" "" { target *-*-* } 0  } */
