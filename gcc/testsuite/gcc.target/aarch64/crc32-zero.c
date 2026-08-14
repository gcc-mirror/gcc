/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+crc" } */

typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;
typedef unsigned short uint16_t;
typedef unsigned char uint8_t;

uint32_t
crc32cb_init_zero(uint16_t x)
{
  return __builtin_aarch64_crc32cb(0, x);
}

uint32_t
crc32cb_data_zero(uint32_t x)
{
  return __builtin_aarch64_crc32cb(x, 0);
}

uint32_t
crc32cb_both_zero(void)
{
  return __builtin_aarch64_crc32cb(0, 0);
}

uint32_t
crc32ch_init_zero(uint16_t x)
{
  return __builtin_aarch64_crc32ch(0, x);
}

uint32_t
crc32ch_data_zero(uint32_t x)
{
  return __builtin_aarch64_crc32ch(x, 0);
}

uint32_t
crc32ch_both_zero(void)
{
  return __builtin_aarch64_crc32ch(0, 0);
}

uint32_t
crc32cw_init_zero(uint32_t x)
{
  return __builtin_aarch64_crc32cw(0, x);
}

uint32_t
crc32cw_data_zero(uint32_t x)
{
  return __builtin_aarch64_crc32cw(x, 0);
}

uint32_t
crc32cw_both_zero(void)
{
  return __builtin_aarch64_crc32cw(0, 0);
}

uint32_t
crc32cx_data_zero(uint32_t x)
{
  return __builtin_aarch64_crc32cx(x, 0);
}

uint32_t
crc32cx_init_zero64(uint64_t x)
{
  return __builtin_aarch64_crc32cx(0, x);
}

uint32_t
crc32cx_both_zero64(void)
{
  return __builtin_aarch64_crc32cx(0, 0);
}

uint32_t
crc32b_init_zero(uint16_t x)
{
  return __builtin_aarch64_crc32b(0, x);
}

uint32_t
crc32b_data_zero(uint32_t x)
{
  return __builtin_aarch64_crc32b(x, 0);
}

uint32_t
crc32b_both_zero(void)
{
  return __builtin_aarch64_crc32b(0, 0);
}

uint32_t
crc32h_init_zero(uint16_t x)
{
  return __builtin_aarch64_crc32h(0, x);
}

uint32_t
crc32h_data_zero(uint32_t x)
{
  return __builtin_aarch64_crc32h(x, 0);
}

uint32_t
crc32h_both_zero(void)
{
  return __builtin_aarch64_crc32h(0, 0);
}

uint32_t
crc32w_init_zero(uint32_t x)
{
  return __builtin_aarch64_crc32w(0, x);
}

uint32_t
crc32w_data_zero(uint32_t x)
{
  return __builtin_aarch64_crc32w(x, 0);
}

uint32_t
crc32w_both_zero(void)
{
  return __builtin_aarch64_crc32w(0, 0);
}

uint32_t
crc32x_data_zero(uint32_t x)
{
  return __builtin_aarch64_crc32x(x, 0);
}

uint32_t
crc32x_init_zero64(uint64_t x)
{
  return __builtin_aarch64_crc32x(0, x);
}

uint32_t
crc32x_both_zero64(void)
{
  return __builtin_aarch64_crc32x(0, 0);
}

/* { dg-final { scan-assembler-times "crc32b\tw\[0-9\]+, wzr, w\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "crc32h\tw\[0-9\]+, wzr, w\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "crc32w\tw\[0-9\]+, wzr, w\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "crc32x\tw\[0-9\]+, wzr, x\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "crc32b\tw\[0-9\]+, w\[0-9\]+, wzr" 1 } } */
/* { dg-final { scan-assembler-times "crc32h\tw\[0-9\]+, w\[0-9\]+, wzr" 1 } } */
/* { dg-final { scan-assembler-times "crc32w\tw\[0-9\]+, w\[0-9\]+, wzr" 1 } } */
/* { dg-final { scan-assembler-times "crc32x\tw\[0-9\]+, w\[0-9\]+, xzr" 1 } } */
/* { dg-final { scan-assembler-times "crc32b\tw\[0-9\]+, wzr, wzr" 1 } } */
/* { dg-final { scan-assembler-times "crc32h\tw\[0-9\]+, wzr, wzr" 1 } } */
/* { dg-final { scan-assembler-times "crc32w\tw\[0-9\]+, wzr, wzr" 1 } } */
/* { dg-final { scan-assembler-times "crc32x\tw\[0-9\]+, wzr, xzr" 1 } } */
/* { dg-final { scan-assembler-times "crc32cb\tw\[0-9\]+, wzr, w\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "crc32ch\tw\[0-9\]+, wzr, w\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "crc32cw\tw\[0-9\]+, wzr, w\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "crc32cx\tw\[0-9\]+, wzr, x\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "crc32cb\tw\[0-9\]+, w\[0-9\]+, wzr" 1 } } */
/* { dg-final { scan-assembler-times "crc32ch\tw\[0-9\]+, w\[0-9\]+, wzr" 1 } } */
/* { dg-final { scan-assembler-times "crc32cw\tw\[0-9\]+, w\[0-9\]+, wzr" 1 } } */
/* { dg-final { scan-assembler-times "crc32cx\tw\[0-9\]+, w\[0-9\]+, xzr" 1 } } */
/* { dg-final { scan-assembler-times "crc32cb\tw\[0-9\]+, wzr, wzr" 1 } } */
/* { dg-final { scan-assembler-times "crc32ch\tw\[0-9\]+, wzr, wzr" 1 } } */
/* { dg-final { scan-assembler-times "crc32cw\tw\[0-9\]+, wzr, wzr" 1 } } */
/* { dg-final { scan-assembler-times "crc32cx\tw\[0-9\]+, wzr, xzr" 1 } } */

/* There should be no moves to a register for zero as it is part of the
 * crc instruction now.
 */
/* { dg-final { scan-assembler-not "mov\t\[wx\]\[0-9\]+, \[wx\]zr" } } */
/* { dg-final { scan-assembler-not "mov\t\[wx\]\[0-9\]+, 0" } } */
