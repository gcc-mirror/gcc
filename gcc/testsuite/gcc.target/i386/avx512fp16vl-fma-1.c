/* { dg-do compile } */
/* { dg-options "-Ofast -mavx512fp16 -mavx512vl" } */

typedef _Float16 v8hf __attribute__ ((__vector_size__ (16)));
typedef _Float16 v16hf __attribute__ ((__vector_size__ (32)));

v8hf
foo1 (v8hf a, v8hf b, v8hf c)
{
  return a * b + c;
}

/* { dg-final { scan-assembler-times "vfmadd132ph\[^\n\r\]*xmm\[0-9\]" 1 } } */

v8hf
foo2 (v8hf a, v8hf b, v8hf c)
{
  return -a * b + c;
}

/* { dg-final { scan-assembler-times "vfnmadd132ph\[^\n\r\]*xmm\[0-9\]" 1 } } */

v8hf
foo3 (v8hf a, v8hf b, v8hf c)
{
  return a * b - c;
}

/* { dg-final { scan-assembler-times "vfmsub132ph\[^\n\r\]*xmm\[0-9\]" 1 } } */

v8hf
foo4 (v8hf a, v8hf b, v8hf c)
{
  return -a * b - c;
}

/* { dg-final { scan-assembler-times "vfnmsub132ph\[^\n\r\]*xmm\[0-9\]" 1 } } */

v16hf
foo5 (v16hf a, v16hf b, v16hf c)
{
  return a * b + c;
}

/* { dg-final { scan-assembler-times "vfmadd132ph\[^\n\r\]*ymm\[0-9\]" 1 } } */

v16hf
foo6 (v16hf a, v16hf b, v16hf c)
{
  return -a * b + c;
}

/* { dg-final { scan-assembler-times "vfnmadd132ph\[^\n\r\]*ymm\[0-9\]" 1 } } */

v16hf
foo7 (v16hf a, v16hf b, v16hf c)
{
  return a * b - c;
}

/* { dg-final { scan-assembler-times "vfmsub132ph\[^\n\r\]*ymm\[0-9\]" 1 } } */

v16hf
foo8 (v16hf a, v16hf b, v16hf c)
{
  return -a * b - c;
}

/* { dg-final { scan-assembler-times "vfnmsub132ph\[^\n\r\]*ymm\[0-9\]" 1 } } */

