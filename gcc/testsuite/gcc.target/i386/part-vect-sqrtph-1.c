/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -Ofast" } */
/* { dg-final { scan-assembler-times {(?n)vsqrtph[ \t].*%xmm[0-9]} 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times {(?n)vsqrtph[ \t].*%xmm[0-9]} 2 { target { ! ia32 } } } } */

void
foo16_sqrt (_Float16* a, _Float16* __restrict c)
{
  c[0] = __builtin_sqrtf16 (a[0]);
  c[1] = __builtin_sqrtf16 (a[1]);
}

void
foo32_sqrt(_Float16* a, _Float16* __restrict c)
{
  c[0] = __builtin_sqrtf16 (a[0]);
  c[1] = __builtin_sqrtf16 (a[1]);
  c[2] = __builtin_sqrtf16 (a[2]);
  c[3] = __builtin_sqrtf16 (a[3]);
}
