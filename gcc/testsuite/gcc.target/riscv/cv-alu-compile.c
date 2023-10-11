/* { dg-do compile } */
/* { dg-require-effective-target cv_alu } */
/* { dg-options "-march=rv32i_xcvalu -mabi=ilp32" } */

#include <stdint.h>

extern int d;
extern int e;
extern int f;

void
foo0(int a, int b)
{
  d = __builtin_riscv_cv_alu_addN (a, b, 0);
  e = __builtin_riscv_cv_alu_addN (a, b, 7);
  f = __builtin_riscv_cv_alu_addN (a, b, 31);
}

void
foo1(int a, int b, int c)
{
  d = __builtin_riscv_cv_alu_addN (a, b, c);
}

void
foo2(int a, int b)
{
  d = __builtin_riscv_cv_alu_addRN (a, b, 0);
  e = __builtin_riscv_cv_alu_addRN (a, b, 7);
  f = __builtin_riscv_cv_alu_addRN (a, b, 31);
}

int
foo3(int a, int b, int c)
{
  return __builtin_riscv_cv_alu_addRN (a, b, c);
}

void
foo4(int a, int b)
{
  d = __builtin_riscv_cv_alu_adduN (a, b, 0);
  e = __builtin_riscv_cv_alu_adduN (a, b, 7);
  f = __builtin_riscv_cv_alu_adduN (a, b, 31);
}

int
foo5(int a, int b, int c)
{
  return __builtin_riscv_cv_alu_adduN (a, b, c);
}

void
foo6(int a, int b)
{
  d = __builtin_riscv_cv_alu_adduRN (a, b, 0);
  e = __builtin_riscv_cv_alu_adduRN (a, b, 7);
  f = __builtin_riscv_cv_alu_adduRN (a, b, 31);
}

int
foo7(int a, int b, int c)
{
  return __builtin_riscv_cv_alu_adduRN (a, b, c);
}

int
foo8(int a, int b)
{
  return __builtin_riscv_cv_alu_clip (a, 15);
}

int
foo9(int a, int b)
{
  return __builtin_riscv_cv_alu_clip (a, 10);
}

int
foo10(int a, int b)
{
  return __builtin_riscv_cv_alu_clipu (a, 15);
}

int
foo11(int a, int b)
{
  return __builtin_riscv_cv_alu_clipu (a, 10);
}

int
foo12(int a)
{
  return __builtin_riscv_cv_alu_extbs (a);
}

int
foo13(int a)
{
  return __builtin_riscv_cv_alu_extbz (a);
}

int
foo14(int b)
{
  return __builtin_riscv_cv_alu_exths (b);
}

int
foo15(int a)
{
  return __builtin_riscv_cv_alu_exthz (a);
}

int
foo16(int a, int b)
{
  return __builtin_riscv_cv_alu_max (a, b);
}

int
foo17(int a, int b)
{
  return __builtin_riscv_cv_alu_maxu (a, b);
}

int
foo18(int a, int b)
{
  return __builtin_riscv_cv_alu_min (a, b);
}

int
foo19(int a, int b)
{
  return __builtin_riscv_cv_alu_minu (a, b);
}

int
foo20(int a, int b)
{
  return __builtin_riscv_cv_alu_slet (a, b);
}

int
foo21(unsigned int a, unsigned int b)
{
  return __builtin_riscv_cv_alu_sletu (a, b);
}

void
foo22(int a, int b)
{
  d = __builtin_riscv_cv_alu_subN (a, b, 0);
  e = __builtin_riscv_cv_alu_subN (a, b, 7);
  f = __builtin_riscv_cv_alu_subN (a, b, 31);
}

int
foo23(int a, int b, int c)
{
  return __builtin_riscv_cv_alu_subN (a, b, c);
}

void
foo24(int a, int b)
{
  d = __builtin_riscv_cv_alu_subRN (a, b, 0);
  e = __builtin_riscv_cv_alu_subRN (a, b, 7);
  f = __builtin_riscv_cv_alu_subRN (a, b, 31);
}

int
foo25(int a, int b, int c)
{
  return __builtin_riscv_cv_alu_subRN (a, b, c);
}

void
foo26(int a, int b)
{
  d = __builtin_riscv_cv_alu_subuN (a, b, 0);
  e = __builtin_riscv_cv_alu_subuN (a, b, 7);
  f = __builtin_riscv_cv_alu_subuN (a, b, 31);
}

int
foo27(int a, int b, int c)
{
  return __builtin_riscv_cv_alu_subuN (a, b, c);
}

void
foo28(int a, int b)
{
  d = __builtin_riscv_cv_alu_subuRN (a, b, 0);
  e = __builtin_riscv_cv_alu_subuRN (a, b, 7);
  f = __builtin_riscv_cv_alu_subuRN (a, b, 31);
}

int
foo29(int a, int b, int c)
{
  return __builtin_riscv_cv_alu_subuRN (a, b, c);
}

/* { dg-final { scan-assembler-times "cv\.addn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0" 1 { target { no-opts "-O1" no-opts "-O2" no-opts "-O3" no-opts "-Og" no-opts "-Oz" no-opts "-Os" } } } } */
/* { dg-final { scan-assembler-times "cv\.addn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.addn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),31" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.addnr\t" 1 } } */
/* { dg-final { scan-assembler-times "cv\.addrn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.addrn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.addrn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),31" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.addrnr\t" 1 } } */
/* { dg-final { scan-assembler-times "cv\.addun\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0" 1 { target { no-opts "-O1" no-opts "-O2" no-opts "-O3" no-opts "-Og" no-opts "-Oz" no-opts "-Os" } } } } */
/* { dg-final { scan-assembler-times "cv\.addun\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.addun\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),31" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.addunr\t" 1 } } */
/* { dg-final { scan-assembler-times "cv\.addurn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.addurn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.addurn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),31" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.addurnr\t" 1 } } */
/* { dg-final { scan-assembler-times "cv\.clip\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),5" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.clipr\t" 1 } } */
/* { dg-final { scan-assembler-times "cv\.clipu\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),5" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.clipur\t" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.extbs\t" 1 { target { no-opts "-O1" no-opts "-O2" no-opts "-O3" no-opts "-Og" no-opts "-Oz" no-opts "-Os" } } } } */
/* { dg-final { scan-assembler-times "cv\\.extbz\t" 1 { target { no-opts "-O1" no-opts "-O2" no-opts "-O3" no-opts "-Og" no-opts "-Oz" no-opts "-Os" } } } } */
/* { dg-final { scan-assembler-times "cv\\.exths\t" 1 { target { no-opts "-O1" no-opts "-O2" no-opts "-O3" no-opts "-Og" no-opts "-Oz" no-opts "-Os" } } } } */
/* { dg-final { scan-assembler-times "cv\\.exthz\t" 1 { target { no-opts "-O1" no-opts "-O2" no-opts "-O3" no-opts "-Og" no-opts "-Oz" no-opts "-Os" } } } } */
/* { dg-final { scan-assembler-times "cv\\.max\t" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.maxu\t" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.min\t" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.minu\t" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sle\t" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.sleu\t" 1 } } */
/* { dg-final { scan-assembler-times "cv\.subn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0" 1 { target { no-opts "-O1" no-opts "-O2" no-opts "-O3" no-opts "-Og" no-opts "-Oz" no-opts "-Os" } } } } */
/* { dg-final { scan-assembler-times "cv\.subn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.subn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),31" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.subnr\t" 1 } } */
/* { dg-final { scan-assembler-times "cv\.subrn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.subrn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.subrn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),31" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.subrnr\t" 1 } } */
/* { dg-final { scan-assembler-times "cv\.subun\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0" 1 { target { no-opts "-O1" no-opts "-O2" no-opts "-O3" no-opts "-Og" no-opts "-Oz" no-opts "-Os" } } } } */
/* { dg-final { scan-assembler-times "cv\.subun\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.subun\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),31" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.subunr\t" 1 } } */
/* { dg-final { scan-assembler-times "cv\.suburn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),0" 1 } } */
/* { dg-final { scan-assembler-times "cv\.suburn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),7" 1 } } */
/* { dg-final { scan-assembler-times "cv\.suburn\t\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),\(\?\:t\[0-6\]\|a\[0-7\]\|s\[1-11\]\),31" 1 } } */
/* { dg-final { scan-assembler-times "cv\\.suburnr\t" 1 } } */
