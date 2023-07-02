/* PR tree-optimization/110508 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (unsigned a, unsigned b, unsigned *c, _Bool d)
{
  __builtin_addc (a, b, d, c);
}
