/* PR tree-optimization/113459 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

#if __BITINT_MAXWIDTH__ >= 129
# define N 129
#else
# define N 63
#endif

_BitInt(N) a;

_BitInt(N)
foo (void)
{
  __builtin_memset (&a, 6, sizeof a);
  return a;
}
