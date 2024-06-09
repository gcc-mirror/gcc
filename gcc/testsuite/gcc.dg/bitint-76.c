/* PR tree-optimization/113462 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

#if __BITINT_MAXWIDTH__ >= 129
typedef _BitInt(129) B;
#else
typedef _BitInt(63) B;
#endif

B
foo (void)
{
  struct { B b; } s = {};
  return s.b;
}
