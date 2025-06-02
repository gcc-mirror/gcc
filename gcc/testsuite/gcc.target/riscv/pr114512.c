/* { dg-do compile } */
/* { dg-options "-march=rv64gcb -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-march=rv32gcb -mabi=ilp32" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */

/* We need to adjust the constant so this works for rv32 and rv64.  */
#if __riscv_xlen == 32
#define ONE 1U
#define MASK 0x1f
typedef unsigned int utype;
#else
#define ONE 1ULL
#define MASK 0x3f
typedef unsigned long utype;
#endif


_Bool my_isxdigit_1(unsigned char ch) {
  utype mask1 = 0x03FF007E;
  if (!((mask1 >> (ch & MASK)) & 1))
    return 0;

  return 1;
}

_Bool my_isxdigit_1a(unsigned char ch) {
  utype mask2 = 0x58;
  if (!((mask2 >> (ch >> 4)) & 1))
    return 0;

  return 1;
}

_Bool my_isxdigit_2(unsigned char ch) {
  utype mask1 = 0x03FF007E;
  if (!(mask1 & (ONE << (ch & MASK))))
    return 0;

  return 1;
}

_Bool my_isxdigit_2a(unsigned char ch) {
  utype mask2 = 0x58;
  if (!(mask2 & (ONE << (ch >> 4))))
    return 0;

  return 1;
}

_Bool my_isxdigit_1_parm(unsigned char ch, utype mask1) {
  if (!((mask1 >> (ch & MASK)) & 1))
    return 0;

  return 1;
}

_Bool my_isxdigit_1a_parm(unsigned char ch, utype mask2) {
  if (!((mask2 >> (ch >> 4)) & 1))
    return 0;

  return 1;
}

_Bool my_isxdigit_2_parm(unsigned char ch, utype mask1) {
  if (!(mask1 & (ONE << (ch & MASK))))
    return 0;

  return 1;
}

_Bool my_isxdigit_2a_parm(unsigned char ch, utype mask2) {
  if (!(mask2 & (ONE << (ch >> 4))))
    return 0;

  return 1;
}

/* Each test should generate a single bext.  */
/* { dg-final { scan-assembler-times "bext\t" 8 } } */
