/* { dg-do compile } */
/* { dg-options "-O2 -mfma -mfpmath=sse" } */

typedef float v1sf __attribute__((vector_size(4)));

v1sf
foo43 (v1sf a, v1sf b, v1sf c)
{
  return a * b + c;
}

/* { dg-final { scan-assembler "fmadd" } } */
