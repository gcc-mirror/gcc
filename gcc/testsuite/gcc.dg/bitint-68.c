/* PR tree-optimization/113323 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-std=c23 -O2" } */

typedef long __attribute__((__vector_size__ (16))) V;
V u, v;
_BitInt(535) i;

void
foo (void)
{
  while (i)
    u = v;
}
