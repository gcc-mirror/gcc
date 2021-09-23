/* PR tree-optimization/102141 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned int __attribute__((__vector_size__ (4))) v;

void
foo (unsigned long long x)
{
  v &= (unsigned) (x >> 56 | x >> 40 & 0xff00);
}
