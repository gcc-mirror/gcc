/* { dg-do compile } */
/* { dg-options "-O -mcpu=ultrasparc -mvis" } */
typedef short vec16 __attribute__((vector_size(8)));

vec16 foo(vec16 a, vec16 b)
{
  return a + b;
}

/* { dg-final { scan-assembler "fpadd16\t%" } } */
