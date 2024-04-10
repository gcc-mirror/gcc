/* PR tree-optimization/113639 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=c23" } */

int j, k;
#if __BITINT_MAXWIDTH__ >= 162
struct S { _BitInt(162) n; };
void bar (_BitInt(162) x);

void
foo (struct S s)
{
  bar (s.n * j);
  (void) (s.n * k);
}
#endif
