/* PR middle-end/116135 */
/* { dg-do compile } */
/* { dg-require-effective-target bitint } */
/* { dg-options "-O2 -std=c23" } */

int
foo (unsigned _BitInt(31) x, unsigned _BitInt(31) y,
     unsigned _BitInt(31) *res)
{
  return __builtin_mul_overflow (x, y, res);
}

/* { dg-final { scan-assembler-times {\tcmp\txzr, x[0-9]+, lsr 31} 1 } } */
/* { dg-final { scan-assembler-not {\tcmp\txzr, x[0-9]+, lsr 32} } } */
