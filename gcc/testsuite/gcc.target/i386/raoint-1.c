/* { dg-do compile } */
/* { dg-options "-mraoint -O2" } */
/* { dg-final { scan-assembler-times "aadd" 2 { target {! ia32 } } } } */
/* { dg-final { scan-assembler-times "aand" 2 { target {! ia32 } } } } */
/* { dg-final { scan-assembler-times "aor" 2 { target {! ia32 } } } } */
/* { dg-final { scan-assembler-times "axor" 2 { target {! ia32 } } } } */
/* { dg-final { scan-assembler-times "aadd" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "aand" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "aor" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "axor" 1 { target ia32 } } } */
#include <immintrin.h>

volatile int x;
volatile long long y;
int *a;
long long *b;

void extern
rao_int_test (void)
{
  _aadd_i32 (a, x);
  _aand_i32 (a, x);
  _aor_i32 (a, x);
  _axor_i32 (a, x);
#ifdef __x86_64__
  _aadd_i64 (b, y);
  _aand_i64 (b, y);
  _aor_i64 (b, y);
  _axor_i64 (b, y);
#endif
}
