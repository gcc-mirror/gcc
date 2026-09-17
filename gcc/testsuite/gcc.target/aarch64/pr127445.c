/* PR rtl-optimization/127445 */
/* { dg-do compile } */
/* { dg-options "-O2 -mnarrow-gp-writes -fzero-call-used-regs=used-arg" } */

/* One return path of foo is a sibling call and the other is a plain return.
   The zeroing sequence emitted on the plain return path adds the argument
   registers to the exit block use set, which changes the set of registers
   that the sibling call is recorded as clobbering.  used-arg only zeroes
   registers that are already live, so nothing else forces a rescan of the
   call to keep its data flow information correct.

   The stale refs are only diagnosed by a compiler configured with
   --enable-checking=df, which verifies them in df_analyze.  */

int bar (int, int, int, int, int, int, int, int);

int
foo (int a, int b, int c, int d, int e, int f, int g, int h)
{
  if (a)
    return bar (a, b, c, d, e, f, g, h);
  return a + b + c + d + e + f + g + h;
}
