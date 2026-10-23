/* { dg-do compile { target ia32 } } */
/* { dg-additional-options "-O2 -std=c99 -fdump-tree-optimized" } */

#include <stdint.h>

uint64_t crc64_data8 ()
{
  return __builtin_crc64_data8 (0xffffffffffffffffULL, 0x32,
				0x40021234002123ULL);
}

uint64_t crc64_data16 ()
{
  return __builtin_crc64_data16 (0xffffffffffffffffULL, 0x3232,
				 0x40021234002123ULL);
}

uint64_t crc64_data32 ()
{
  return __builtin_crc64_data32 (0xffffffffffffffffULL, 0x123546ff,
				 0x40021234002123ULL);
}

uint64_t crc64_data64 ()
{
  return __builtin_crc64_data64 (0xffffffffffffffffULL, 0x123546ff123546ffULL,
				 0x40021234002123ULL);
}

uint64_t rev_crc64_data8 ()
{
  return __builtin_rev_crc64_data8 (0xffffffffffffffffULL, 0x32,
				    0x40021234002123ULL);
}

uint64_t rev_crc64_data16 ()
{
  return __builtin_rev_crc64_data16 (0xffffffffffffffffULL, 0x3232,
				     0x40021234002123ULL);
}

uint64_t rev_crc64_data32 ()
{
  return __builtin_rev_crc64_data32 (0xffffffffffffffffULL, 0x123546ff,
				     0x40021234002123ULL);
}

uint64_t rev_crc64_data64 ()
{
  return __builtin_rev_crc64_data64 (0xffffffffffffffffULL,
				     0x123546ff123546ffULL,
				     0x40021234002123ULL);
}

/* Test that crc64 builtins polynomial arg doesn't get truncated
   to 32-bit on ia32 and computes expected values.  */

/* { dg-final { scan-tree-dump-times "return 14753344481753729783;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 9361103135052257218;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 2386776299826502735;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 16682003383967693607;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 12528055141939117259;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 9375109733926652568;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 13461549257995699439;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 16230122087500358547;" 1 "optimized" } } */
