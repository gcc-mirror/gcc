/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -Ofast" } */
/* { dg-final { scan-assembler-times {(?n)vmaxph[ \t].*%xmm[0-9]} 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times {(?n)vminph[ \t].*%xmm[0-9]} 2 { target { ! ia32 } } } } */

void
foo16_max (_Float16* a, _Float16* b, _Float16* __restrict c)
{
  c[0] = __builtin_fmaxf16 (a[0], b[0]);
  c[1] = __builtin_fmaxf16 (a[1], b[1]);
}

void
foo32_max(_Float16* a, _Float16* b, _Float16* __restrict c)
{
  c[0] = __builtin_fmaxf16 (a[0], b[0]);
  c[1] = __builtin_fmaxf16 (a[1], b[1]);
  c[2] = __builtin_fmaxf16 (a[2], b[2]);
  c[3] = __builtin_fmaxf16 (a[3], b[3]);
}

void
foo16_min (_Float16* a, _Float16* b, _Float16* __restrict c)
{
  c[0] = __builtin_fminf16 (a[0], b[0]);
  c[1] = __builtin_fminf16 (a[1], b[1]);
}

void
foo32_min(_Float16* a, _Float16* b, _Float16* __restrict c)
{
  c[0] = __builtin_fminf16 (a[0], b[0]);
  c[1] = __builtin_fminf16 (a[1], b[1]);
  c[2] = __builtin_fminf16 (a[2], b[2]);
  c[3] = __builtin_fminf16 (a[3], b[3]);
}
