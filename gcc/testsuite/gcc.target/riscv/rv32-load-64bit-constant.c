/* { dg-do compile } */
/* { dg-options "-march=rv32im -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */


/* This test only applies to RV32. Some of 64bit constants in this test will be put
into the constant pool in RV64, since RV64 might need one extra instruction to load
64bit constant. */

unsigned long long
rv32_mov_64bit_int1 (void)
{
  return 0x739290001LL;
}

unsigned long long
rv32_mov_64bit_int2 (void)
{
  return 0x839290001LL;
}

unsigned long long
rv32_mov_64bit_int3 (void)
{
  return 0x3929000139290000LL;
}

unsigned long long
rv32_mov_64bit_int4 (void)
{
  return 0x3929001139290000LL;
}

unsigned long long
rv32_mov_64bit_int5 (void)
{
  return 0x14736def39290000LL;
}

/* { dg-final { scan-assembler-not "lw\t" } } */
