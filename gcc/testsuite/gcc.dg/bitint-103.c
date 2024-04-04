/* PR tree-optimization/114555 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

#if __BITINT_MAXWIDTH__ >= 1225
struct S { _BitInt(512) : 98; _BitInt(1225) b : 509; } s;
_BitInt(1225) a;
#endif

void
foo (void)
{
#if __BITINT_MAXWIDTH__ >= 1225
  a ^= (unsigned _BitInt(1025)) s.b;
#endif
}
