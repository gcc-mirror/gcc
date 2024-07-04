/* PR tree-optimization/113330 */
/* { dg-do compile { target bitint } } */
/* { dg-require-stack-check "generic" } */
/* { dg-options "-std=c23 -O --param=large-stack-frame=131072 -fstack-check=generic --param=sccvn-max-alias-queries-per-access=0" } */

_BitInt(8) a;

static inline __attribute__((__always_inline__)) void
bar (int, int, int, int, int, int, int, int)
{
#if __BITINT_MAXWIDTH__ >= 65535
  _BitInt(65535) b = 0;
  _BitInt(383) c = 0;
#else
  _BitInt(63) b = 0;
  _BitInt(39) c = 0;
#endif
  a = b;
}

void
foo (void)
{
  bar (0, 0, 0, 0, 0, 0, 0, 0);
}
