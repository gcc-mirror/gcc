/* PR tree-optimization/113408 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

#if __BITINT_MAXWIDTH__ >= 713
struct A { _BitInt(713) b; } g;
#else
struct A { _BitInt(49) b; } g;
#endif
int f;

void
foo (void)
{
  struct A j = g;
  if (j.b)
    f = 0;
}
