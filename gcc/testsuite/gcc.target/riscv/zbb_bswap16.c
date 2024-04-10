/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64d"  { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zbb -mabi=ilp32d" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

int foo(int n)
{
  return __builtin_bswap16(n);
}

/* { dg-final { scan-assembler {\mrev8} } } */
/* { dg-final { scan-assembler {\msrli} } } */

