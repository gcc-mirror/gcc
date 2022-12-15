/* PR target/107546 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-xop -mno-avx512f" } */
/* { dg-final { scan-assembler-not "pcmpeqb\t" } } */
/* { dg-final { scan-assembler-times "pcmpgtb\t" 2 } } */

typedef signed char V __attribute__((vector_size(16)));

V
foo (V x)
{
  return x < 48;
}

V
bar (V x)
{
  return x >= (V) { 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57 };
}
