/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */
/* { dg-final { scan-assembler-times "vfmadd132ph\[^\n\r\]*xmm\[0-9\]" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vfnmadd132ph\[^\n\r\]*xmm\[0-9\]" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vfmsub132ph\[^\n\r\]*xmm\[0-9\]" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vfnmsub132ph\[^\n\r\]*xmm\[0-9\]" 2 { target { ! ia32 } } } } */

typedef _Float16 v4hf __attribute__ ((__vector_size__ (8)));
typedef _Float16 v2hf __attribute__ ((__vector_size__ (4)));

v4hf
fma_v4hf (v4hf a, v4hf b, v4hf c)
{
  return a * b + c;
}

v4hf
fnma_v4hf (v4hf a, v4hf b, v4hf c)
{
  return -a * b + c;
}

v4hf
fms_v4hf (v4hf a, v4hf b, v4hf c)
{
  return a * b - c;
}

v4hf
fnms_v4hf (v4hf a, v4hf b, v4hf c)
{
  return -a * b - c;
}

v2hf
fma_v2hf (v2hf a, v2hf b, v2hf c)
{
  return a * b + c;
}

v2hf
fnma_v2hf (v2hf a, v2hf b, v2hf c)
{
  return -a * b + c;
}

v2hf
fms_v2hf (v2hf a, v2hf b, v2hf c)
{
  return a * b - c;
}

v2hf
fnms_v2hf (v2hf a, v2hf b, v2hf c)
{
  return -a * b - c;
}

