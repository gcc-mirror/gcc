/* PR tree-optimization/114329 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23" } */

#if __BITINT_MAXWIDTH__ >= 129
#define N 129
#else
#define N 63
#endif

struct S { _BitInt(N) b : N; } s;

void
foo (void)
{
  s.b ^= 42;
}
