/* { dg-do compile } */
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


/* Test that builtin calls are optimized away and not present */
/* { dg-final { scan-tree-dump-not "__builtin_crc8_data8" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_crc16_data8" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_crc16_data16" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_crc32_data8" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_crc32_data16" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_crc32_data32" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_rev_crc8_data8" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_rev_crc16_data8" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_rev_crc16_data16" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_rev_crc32_data8" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_rev_crc32_data16" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_rev_crc32_data32" "optimized" } } */

/* Test that the builtins are folded to the expected values. */
/* { dg-final { scan-tree-dump-times "return 160;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 31476;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 8836;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 3353799058;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1056422874;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 2325894126;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 74;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1338;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 561;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 3483277276;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1523773068;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 340857104;" 1 "optimized" } } */
