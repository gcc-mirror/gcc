/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target int32plus } */
/* { dg-additional-options "-O2 -fdump-tree-optimized" } */

#include <stdint.h>

uint8_t crc8_data8 ()
{
  return __builtin_crc8_data8 (0x34, 'a', 0x12);
}

uint16_t crc16_data8 ()
{
  return __builtin_crc16_data8 (0x1234, 'a', 0x1021);
}

uint16_t crc16_data16 ()
{
  return __builtin_crc16_data16 (0x1234, 0x3214, 0x1021);
}

uint32_t crc32_data8 ()
{
  return __builtin_crc32_data8 (0xffffffff, 0x32, 0x4002123);
}

uint32_t crc32_data16 ()
{
  return __builtin_crc32_data16 (0xffffffff, 0x3232, 0x4002123);
}

uint32_t crc32_data32 ()
{
  return __builtin_crc32_data32 (0xffffffff, 0x123546ff, 0x4002123);
}

uint64_t crc64_data8 ()
{
  return __builtin_crc64_data8 (0xffffffffffffffff, 0x32, 0x40021234002123);
}

uint64_t crc64_data16 ()
{
  return __builtin_crc64_data16 (0xffffffffffffffff, 0x3232, 0x40021234002123);
}

uint64_t crc64_data32 ()
{
  return __builtin_crc64_data32 (0xffffffffffffffff, 0x123546ff,
				 0x40021234002123);
}

uint64_t crc64_data64 ()
{
  return __builtin_crc64_data64 (0xffffffffffffffff, 0x123546ff123546ff,
				 0x40021234002123);
}

uint8_t rev_crc8_data8 ()
{
  return __builtin_rev_crc8_data8 (0x34, 'a', 0x12);
}

uint16_t rev_crc16_data8 ()
{
  return __builtin_rev_crc16_data8 (0x1234, 'a', 0x1021);
}

uint16_t rev_crc16_data16 ()
{
  return __builtin_rev_crc16_data16 (0x1234, 0x3214, 0x1021);
}

uint32_t rev_crc32_data8 ()
{
  return __builtin_rev_crc32_data8 (0xffffffff, 0x32, 0x4002123);
}

uint32_t rev_crc32_data16 ()
{
  return __builtin_rev_crc32_data16 (0xffffffff, 0x3232, 0x4002123);
}

uint32_t rev_crc32_data32 ()
{
  return __builtin_rev_crc32_data32 (0xffffffff, 0x123546ff, 0x4002123);
}

uint64_t rev_crc64_data8 ()
{
  return __builtin_rev_crc64_data8 (0xffffffffffffffff, 0x32,
				    0x40021234002123);
}

uint64_t rev_crc64_data16 ()
{
  return __builtin_rev_crc64_data16 (0xffffffffffffffff, 0x3232,
				     0x40021234002123);
}

uint64_t rev_crc64_data32 ()
{
  return __builtin_rev_crc64_data32 (0xffffffffffffffff, 0x123546ff,
				     0x40021234002123);
}

uint64_t rev_crc64_data64 ()
{
  return __builtin_rev_crc64_data64 (0xffffffffffffffff, 0x123546ff123546ff,
				     0x40021234002123);
}


/* Test that builtin calls are optimized away and not present */
/* { dg-final { scan-tree-dump-not "__builtin_crc8_data8" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_crc16_data8" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_crc16_data16" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_crc32_data8" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_crc32_data16" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_crc32_data32" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_crc64_data8" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_crc64_data16" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_crc64_data32" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_crc64_data64" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_rev_crc8_data8" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_rev_crc16_data8" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_rev_crc16_data16" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_rev_crc32_data8" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_rev_crc32_data16" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_rev_crc32_data32" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_rev_crc64_data8" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_rev_crc64_data16" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_rev_crc64_data32" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_rev_crc64_data64" "optimized" } } */

/* Test that the builtins are folded to the expected values. */
/* { dg-final { scan-tree-dump-times "return 160;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 31476;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 8836;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 3353799058;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1056422874;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 2325894126;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 14753344481753729783;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 9361103135052257218;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 2386776299826502735;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 16682003383967693607;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 74;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1338;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 561;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 3483277276;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1523773068;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 340857104;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 12528055141939117259;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 9375109733926652568;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 13461549257995699439;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 16230122087500358547;" 1 "optimized" } } */
