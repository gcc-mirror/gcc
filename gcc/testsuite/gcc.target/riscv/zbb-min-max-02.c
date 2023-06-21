/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba_zbb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

int f(unsigned int* a)
{
  const int C = 1000;
  return *a * 3 > C ? C : *a * 3;
}

/* { dg-final { scan-assembler-times "minu" 1 } } */
/* { dg-final { scan-assembler-not "sext.w" } } */
/* { dg-final { scan-assembler-not "zext.w" } } */

