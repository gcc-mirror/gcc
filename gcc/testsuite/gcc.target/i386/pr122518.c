/* PR target/122518 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

inline unsigned min (unsigned a, unsigned b)
{
  return (a < b) ? a : b;
}

unsigned uminsub (unsigned a, unsigned b)
{
  return min (a - b, a);
}

/* { dg-final { scan-assembler-not "cmp" } } */
