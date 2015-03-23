/* { dg-do compile } */
/* { dg-options "-O2 -fipa-ra -fomit-frame-pointer -fno-optimize-sibling-calls" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */

/* Test -fipa-ra optimization on self-recursive function.  */

static int __attribute__((noinline))
bar (int x)
{
  if (x > 4)
    return bar (x - 3);
  return 0;
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

/* Check that addition uses dx. */
/* { dg-final { scan-assembler-times "addl\t%\[re\]?dx, %\[re\]?ax" 1 { xfail { { ! nonpic } && ia32 } } } } */

/* Verify that bar is self-recursive.  */
/* { dg-final { scan-assembler-times "call\t_?bar" 2 } } */
