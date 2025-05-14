/* { dg-do compile } */
/* { dg-options "-fdump-tree-einline" } */

/* PR ipa/119599 */
/* inlining a noreturn function which returns
   can cause an ICE when dealing finding an unreachable block.
   We should get a __builtin_unreachable after the inliing.  */


void baz (void);

static inline __attribute__((always_inline, noreturn)) void
bar (void)
{
  static volatile int t = 0;
  if (t == 0)
    baz ();
} /* { dg-warning "function does return" } */

void
foo (void)
{
  bar ();
}

/* After inlining, we should have call to __builtin_unreachable now. */
/* { dg-final { scan-tree-dump "__builtin_unreachable " "einline" } } */
