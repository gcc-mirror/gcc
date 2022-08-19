/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

int foo(int n)
{
  return __builtin_bswap32(n);
}

/* { dg-final { scan-assembler "rev8" } } */

