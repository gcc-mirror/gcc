/* { dg-do compile } */
/* { dg-options "-O -mcpu=ultrasparc -mvis" } */
typedef short vec16 __attribute__((vector_size(8)));
typedef int vec32 __attribute__((vector_size(8)));

vec16 fun16(vec16 a, vec16 b)
{
  return (~a & b) + (b | a) - (a ^ b);
}

vec32 fun32(vec32 a, vec32 b)
{
  return (~a & b) + (b | a) - (a ^ b);
}

/* This should be transformed into ~b & a.  */
vec16 fun16b(vec16 a, vec16 b)
{
  return (a & ~b) + (b | a) - (a ^ b);
}

vec32 fun32b(vec32 a, vec32 b)
{
  return (a & ~b) + (b | a) - (a ^ b);
}

/* { dg-final { scan-assembler-times "fandnot1\t%" 4 } } */
/* { dg-final { scan-assembler-times "for\t%" 4 } } */
/* { dg-final { scan-assembler-times "fpadd" 4 } } */
/* { dg-final { scan-assembler-times "fxor\t%" 4 } } */
/* { dg-final { scan-assembler-times "fpsub" 4 } } */
