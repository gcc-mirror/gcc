/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+crc -fdump-tree-optimized" } */

unsigned int
test_crc32_u8_1 ()
{
  return __builtin_aarch64_crc32b (0x02a24bf8, 0xba);
}

unsigned int
test_crc32_u8_2 ()
{
  return __builtin_aarch64_crc32b (0x0cd5e10f, 0xe7);
}

unsigned int
test_crc32_u8_3 ()
{
  return __builtin_aarch64_crc32b (0x50e739b8, 0x2e);
}

unsigned int
test_crc32_u8_4 ()
{
  return __builtin_aarch64_crc32b (0x3c2db116, 0x3d);
}

unsigned int
test_crc32_u8_5 ()
{
  return __builtin_aarch64_crc32b (-1, -1);
}

unsigned int
test_crc32_u16_1 ()
{
  return __builtin_aarch64_crc32h (0x02a24bf8, 0xd4ba);
}

unsigned int
test_crc32_u16_2 ()
{
  return __builtin_aarch64_crc32h (0x0cd5e10f, 0x42e7);
}

unsigned int
test_crc32_u16_3 ()
{
  return __builtin_aarch64_crc32h (0x50e739b8, 0x382e);
}

unsigned int
test_crc32_u16_4 ()
{
  return __builtin_aarch64_crc32h (0x3c2db116, 0x7f3d);
}

unsigned int
test_crc32_u16_5 ()
{
  return __builtin_aarch64_crc32h (-1, -1);
}

unsigned int
test_crc32_u32_1 ()
{
  return __builtin_aarch64_crc32w (0x02a24bf8, 0xf37dd4ba);
}

unsigned int
test_crc32_u32_2 ()
{
  return __builtin_aarch64_crc32w (0x0cd5e10f, 0x832b42e7);
}

unsigned int
test_crc32_u32_3 ()
{
  return __builtin_aarch64_crc32w (0x50e739b8, 0xcefb382e);
}

unsigned int
test_crc32_u32_4 ()
{
  return __builtin_aarch64_crc32w (0x3c2db116, 0xd3947f3d);
}

unsigned int
test_crc32_u32_5 ()
{
  return __builtin_aarch64_crc32w (-1, -1);
}

unsigned int
test_crc32_u64_1 ()
{
  return __builtin_aarch64_crc32x (0x02a24bf8, 0x894a58bff37dd4ba);
}

unsigned int
test_crc32_u64_2 ()
{
  return __builtin_aarch64_crc32x (0x0cd5e10f, 0x24334e2e832b42e7);
}

unsigned int
test_crc32_u64_3 ()
{
  return __builtin_aarch64_crc32x (0x50e739b8, 0x289ee1b7cefb382e);
}

unsigned int
test_crc32_u64_4 ()
{
  return __builtin_aarch64_crc32x (0x3c2db116, 0xa89143dad3947f3d);
}

unsigned int
test_crc32_u64_5 ()
{
  return __builtin_aarch64_crc32x (-1, -1);
}

/* Test that there is no crc32 instruction emitted.  */
/* { dg-final { scan-assembler-not "crc32\[bhwx\]" } } */

/* Test that the builtins are folded to the expected values. */
/* { dg-final { scan-tree-dump-times "return 2563801847;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 2933768107;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 423416136;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 2894124273;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 16777215;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 2734662128;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 3648554851;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1344032237;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 179056371;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 65535;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 189297697;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1070454892;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1747531601;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 3920656749;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 0;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1565043101;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 3369685291;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 2806525577;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 605909632;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 3736805603;" 1 "optimized" } } */
