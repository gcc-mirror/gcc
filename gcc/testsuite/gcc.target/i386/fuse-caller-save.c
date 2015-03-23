/* { dg-do compile } */
/* { dg-options "-O2 -fipa-ra -fomit-frame-pointer" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */

/* Testing -fipa-ra optimization option.  */

static int __attribute__((noinline))
bar (int x)
{
  return x + 3;
}

int __attribute__((noinline))
foo (int y)
{
  return y + bar (y);
}

/* For !nonpic && ia32 xfails, see PR64895.  */

/* Check that no registers are saved/restored. */
/* { dg-final { scan-assembler-not "push" { xfail { { ! nonpic } && ia32 } } } } */
/* { dg-final { scan-assembler-not "pop" { xfail { { ! nonpic } && ia32 } } } } */

/* PR61605.  If the first argument register and the return register differ, then
   bar leaves the first argument register intact.  That means in foo that the
   first argument register still contains y after bar has been called, and
   there's no need to copy y to a different register before the call, to be able
   to use it after the call.
   Check that the copy is absent.  */
/* { dg-final { scan-assembler-not "movl" { target { ! ia32 } } } } */

/* Check that addition uses di (in case of no copy) or dx (in case of copy). */
/* { dg-final { scan-assembler-times "addl\t%\[re\]?d\[ix\], %\[re\]?ax" 1 { xfail { { ! nonpic } && ia32 } } } } */
