/* { dg-do compile } */
/* { dg-options "-O -mcpu=ultrasparc -mvis" } */
typedef short vec16 __attribute__((vector_size(4)));

vec16 foo(vec16 a, vec16 b)
{
  return a - b;
}

/* { dg-final { scan-assembler "fpsub16s\t%" } } */
