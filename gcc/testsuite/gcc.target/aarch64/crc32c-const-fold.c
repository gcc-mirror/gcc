/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+crc -fdump-tree-optimized" } */

unsigned int
test_crc32c_u8_1 ()
{
  return __builtin_aarch64_crc32cb (0x02a24bf8, 0xba);
}

unsigned int
test_crc32c_u8_2 ()
{
  return __builtin_aarch64_crc32cb (0x0cd5e10f, 0xe7);
}

unsigned int
test_crc32c_u8_3 ()
{
  return __builtin_aarch64_crc32cb (0x50e739b8, 0x2e);
}

unsigned int
test_crc32c_u8_4 ()
{
  return __builtin_aarch64_crc32cb (0x3c2db116, 0x3d);
}

unsigned int
test_crc32c_u8_5 ()
{
  return __builtin_aarch64_crc32cb (-1, -1);
}

unsigned int
test_crc32c_u16_1 ()
{
  return __builtin_aarch64_crc32ch (0x02a24bf8, 0xd4ba);
}

unsigned int
test_crc32c_u16_2 ()
{
  return __builtin_aarch64_crc32ch (0x0cd5e10f, 0x42e7);
}

unsigned int
test_crc32c_u16_3 ()
{
  return __builtin_aarch64_crc32ch (0x50e739b8, 0x382e);
}

unsigned int
test_crc32c_u16_4 ()
{
  return __builtin_aarch64_crc32ch (0x3c2db116, 0x7f3d);
}

unsigned int
test_crc32c_u16_5 ()
{
  return __builtin_aarch64_crc32ch (-1, -1);
}

unsigned int
test_crc32c_u32_1 ()
{
  return __builtin_aarch64_crc32cw (0x02a24bf8, 0xf37dd4ba);
}

unsigned int
test_crc32c_u32_2 ()
{
  return __builtin_aarch64_crc32cw (0x0cd5e10f, 0x832b42e7);
}

unsigned int
test_crc32c_u32_3 ()
{
  return __builtin_aarch64_crc32cw (0x50e739b8, 0xcefb382e);
}

unsigned int
test_crc32c_u32_4 ()
{
  return __builtin_aarch64_crc32cw (0x3c2db116, 0xd3947f3d);
}

unsigned int
test_crc32c_u32_5 ()
{
  return __builtin_aarch64_crc32cw (-1, -1);
}

unsigned int
test_crc32c_u64_1 ()
{
  return __builtin_aarch64_crc32cx (0x02a24bf8, 0x894a58bff37dd4ba);
}

unsigned int
test_crc32c_u64_2 ()
{
  return __builtin_aarch64_crc32cx (0x0cd5e10f, 0x24334e2e832b42e7);
}

unsigned int
test_crc32c_u64_3 ()
{
  return __builtin_aarch64_crc32cx (0x50e739b8, 0x289ee1b7cefb382e);
}

unsigned int
test_crc32c_u64_4 ()
{
  return __builtin_aarch64_crc32cx (0x3c2db116, 0xa89143dad3947f3d);
}

unsigned int
test_crc32c_u64_5 ()
{
  return __builtin_aarch64_crc32cx (-1, -1);
}

/* Test that there is no crc32 instruction emitted.  */
/* { dg-final { scan-assembler-not "crc32c\[bhwx\]" } } */

/* Test that the builtins are folded to the expected values. */
/* { dg-final { scan-tree-dump-times "return 2688732928;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1776624948;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 3025796294;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 3104311380;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 16777215;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 350827643;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1464882880;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1604487598;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 3113037293;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 65535;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 2122844148;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 881727033;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 139192539;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1861871255;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 0;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 3230864714;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 337502055;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 712052683;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1390230251;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 3080238136;" 1 "optimized" } } */
