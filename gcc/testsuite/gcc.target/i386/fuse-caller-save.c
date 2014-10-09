/* { dg-do compile } */
/* { dg-options "-O2 -fuse-caller-save -fomit-frame-pointer" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */

/* Testing -fuse-caller-save optimization option.  */

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

/* Check that no registers are saved/restored. */
/* { dg-final { scan-assembler-not "push"  } } */
/* { dg-final { scan-assembler-not "pop"  } } */

/* Check that addition uses dx. */
/* { dg-final { scan-assembler-times "addl\t%\[re\]?dx, %\[re\]?ax" 1 } } */
