/* { dg-do compile } */
/* { dg-options "-O -mcpu=ultrasparc -mvis" } */
typedef int vec32 __attribute__((vector_size(8)));

vec32 foo(vec32 a, vec32 b)
{
  return a - b;
}

/* { dg-final { scan-assembler "fpsub32\t%" } } */
