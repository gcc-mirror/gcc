/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zbb -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

int foo(int n)
{
  return __builtin_bswap16(n);
}

/* { dg-final { scan-assembler {\mrev8} } } */
/* { dg-final { scan-assembler {\msrli} } } */

