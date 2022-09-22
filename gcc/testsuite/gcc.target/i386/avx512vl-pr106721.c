/* PR target/106721 */
/* { dg-do assemble { target avx512vl } } */
/* { dg-options "-O3 -mavx512vl" } */

typedef __bf16 __m256bf16 __attribute__((__vector_size__(32)));
void (*bar) (__m256bf16, __m256bf16, __m256bf16);
__m256bf16 a;
volatile __bf16 b, c, d, e, f, g, h;

void
foo (void)
{
  __m256bf16 x[8];
  int i;
  for (i = 0; i < 8; i++)
    x[i] = (__m256bf16) { b, c, d, e, f, g, h };
  a = x[6];
  bar (x[0], x[6], x[7]);
}
