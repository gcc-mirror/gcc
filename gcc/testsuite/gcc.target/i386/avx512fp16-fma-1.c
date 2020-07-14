/* { dg-do compile } */
/* { dg-options "-Ofast -mavx512fp16" } */

typedef _Float16 v32hf __attribute__ ((__vector_size__ (64)));

_Float16
foo1 (_Float16 a, _Float16 b, _Float16 c)
{
  return a * b + c;
}

/* { dg-final { scan-assembler-times "vfmadd132sh\[^\n\r\]*xmm\[0-9\]" 1 } } */

_Float16
foo2 (_Float16 a, _Float16 b, _Float16 c)
{
  return -a * b + c;
}

/* { dg-final { scan-assembler-times "vfnmadd132sh\[^\n\r\]*xmm\[0-9\]" 1 } } */

_Float16
foo3 (_Float16 a, _Float16 b, _Float16 c)
{
  return a * b - c;
}

/* { dg-final { scan-assembler-times "vfmsub132sh\[^\n\r\]*xmm\[0-9\]" 1 } } */

_Float16
foo4 (_Float16 a, _Float16 b, _Float16 c)
{
  return -a * b - c;
}

/* { dg-final { scan-assembler-times "vfnmsub132sh\[^\n\r\]*xmm\[0-9\]" 1 } } */

v32hf
foo5 (v32hf a, v32hf b, v32hf c)
{
  return a * b + c;
}

/* { dg-final { scan-assembler-times "vfmadd132ph\[^\n\r\]*zmm\[0-9\]" 1 } } */

v32hf
foo6 (v32hf a, v32hf b, v32hf c)
{
  return -a * b + c;
}

/* { dg-final { scan-assembler-times "vfnmadd132ph\[^\n\r\]*zmm\[0-9\]" 1 } } */

v32hf
foo7 (v32hf a, v32hf b, v32hf c)
{
  return a * b - c;
}

/* { dg-final { scan-assembler-times "vfmsub132ph\[^\n\r\]*zmm\[0-9\]" 1 } } */

v32hf
foo8 (v32hf a, v32hf b, v32hf c)
{
  return -a * b - c;
}

/* { dg-final { scan-assembler-times "vfnmsub132ph\[^\n\r\]*zmm\[0-9\]" 1 } } */

