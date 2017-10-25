/* { dg-do compile } */
/* { dg-options "-O -fcf-protection -mcet" } */
/* { dg-final { scan-assembler-times "endbr32" 2 { target ia32 } } } */
/* { dg-final { scan-assembler-times "endbr64" 4 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "rdsspd|incsspd\[ \t]+(%|)eax" { target ia32 } } } */
/* { dg-final { scan-assembler "rdssp\[dq]\[ \t]+(%|)\[re]ax" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "incssp\[dq]\[ \t]+(%|)\[re]di" { target { ! ia32 } } } } */

#include <immintrin.h>

unsigned int f1 ()
{
  unsigned int x = 0;
  return _rdsspd (x);
}

void f3 (unsigned int _a)
{
  _incsspd (_a);
}

#ifdef __x86_64__
unsigned long long f2 ()
{
  unsigned long long x = 0;
  return _rdsspq (x);
}

void f4 (unsigned int _a)
{
  _incsspq (_a);
}
#endif
