/* PR tree-optimization/64563 */
/* { dg-do compile } */
/* { dg-options "-Os -Wtype-limits" } */

int a, b, c, d, f;
unsigned int e;

void
foo (void)
{
  d = b = (a != (e | 4294967288UL));
  if (!d)
    c = f || b;
}
