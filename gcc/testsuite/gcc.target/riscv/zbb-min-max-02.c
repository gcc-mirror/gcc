/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba_zbb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

int f(unsigned int* a)
{
  const int C = 1000;
  return *a * 3 > C ? C : *a * 3;
}

/* { dg-final { scan-assembler-times {\mminu} 1 } } */
/* { dg-final { scan-assembler-not {\msext\.w\M} } } */
/* { dg-final { scan-assembler-not {\mzext\.w\M} } } */

