/* { dg-do compile } */
/* { dg-options "-O2 -fuse-caller-save -fomit-frame-pointer -fno-optimize-sibling-calls" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */

/* Test -fuse-caller-save optimization on self-recursive function.  */

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

int
main (void)
{
  return !(foo (5) == 13);
}

/* Verify that no registers where saved on stack.  */
/* { dg-final { scan-assembler-not "\.cfi_offset"  } } */

/* Verify that bar is self-recursive.  */
/* { dg-final { scan-assembler-times "call\tbar" 2 } } */

