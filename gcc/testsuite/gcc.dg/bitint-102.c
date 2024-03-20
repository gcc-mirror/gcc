/* PR tree-optimization/114365 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

struct S {
  int : 31;
#if __BITINT_MAXWIDTH__ >= 129
  _BitInt(129) b : 129;
#else
  _BitInt(63) b : 63;
#endif
} s;

void
foo (int a)
{
  s.b <<= a;
}
