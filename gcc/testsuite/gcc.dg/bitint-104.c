/* PR tree-optimization/114555 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O -fno-tree-forwprop" } */

#if __BITINT_MAXWIDTH__ >= 4139
struct S { _BitInt(31) : 6; _BitInt(513) b : 241; } s;
_BitInt(4139) a;
#endif

void
foo (void)
{
#if __BITINT_MAXWIDTH__ >= 4139
  int i = 0;
  a -= s.b << i;
#endif
}
