/* { dg-do compile } */
/* { dg-options "-O2 -fuse-caller-save" } */
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

int
main (void)
{
  return !(foo (5) == 13);
}

/* { dg-final { scan-assembler-not "\.cfi_def_cfa_offset"  } } */
/* { dg-final { scan-assembler-not "\.cfi_offset"  } } */

