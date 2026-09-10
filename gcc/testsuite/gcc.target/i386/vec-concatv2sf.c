/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx -masm=att" } */

typedef float v2sf __attribute__((vector_size (8)));

v2sf
f1 (float *a, float *b)
{
  return (v2sf) { *b, *a };
}

/* { dg-final { scan-assembler-times "vinsertps\[ \t]" 1 } } */
/* { dg-final { scan-assembler-not "vunpcklps" } } */

v2sf
f2 (float *a)
{
  return (v2sf) { *a, 0.0 };
}

/* { dg-final { scan-assembler-times "vmovss\[ \t]" 2 } } */
