/* PR target/124294 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx10.2 -masm=intel" } */
/* { dg-require-effective-target masm_intel } */
/* { dg-final { scan-assembler "\tvmovw\txmm0, xmm1" } } */

typedef unsigned char V __attribute__((vector_size (16)));

V
foo (V x, V y)
{
  (void) x;
  return __builtin_shuffle (y, (V) {}, (V) { 0, 1, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 });
}
