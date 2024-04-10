/* PR tree-optimization/113297 */
/* { dg-do compile { target bitint } } */
/* { dg-require-stack-check "generic" } */
/* { dg-options "-std=c23 -O -fno-tree-fre --param=large-stack-frame=1024 -fstack-check=generic" } */

#if __BITINT_MAXWIDTH__ >= 513
typedef _BitInt(513) B;
#else
typedef int B;
#endif

static inline __attribute__((__always_inline__)) void
bar (B x)
{
  B y = x;
  if (y)
    __builtin_abort ();
}

void
foo (void)
{
  bar (0);
}
