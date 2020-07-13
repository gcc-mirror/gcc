/* { dg-do compile } */
/* { dg-options "-O2 -mfpmath=sse -mavx512fp16" } */

int
foo (_Float16 y)
{
  return __builtin_isinf (y);
}

int
foo2 (_Float16 y)
{
  return __builtin_isfinite (y);
}

int
foo3 (_Float16 y)
{
  return __builtin_signbit(y);
}

int
foo4 (_Float16 y)
{
  return __builtin_isnormal (y);
}

/* { dg-final { scan-assembler-not "vcvtsh2s\[sd\]" } }  */
/* { dg-final { scan-assembler-times "vucomish\[^\n\r\]*xmm\[0-9\]" 4 } } */
