/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zbb -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

int foo(int n)
{
  return __builtin_bswap16(n);
}

/* { dg-final { scan-assembler "rev8" } } */
/* { dg-final { scan-assembler "srli" } } */

