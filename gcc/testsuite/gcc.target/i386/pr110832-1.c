/* PR target/110832 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -mno-partial-vector-fp-math" } */

typedef float __attribute__((vector_size(8))) v2sf;

v2sf test (v2sf a, v2sf b)
{
  return a + b;
}

/* { dg-final { scan-assembler-not "addps" } } */
