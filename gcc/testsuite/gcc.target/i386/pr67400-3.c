/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -fno-pic -fno-plt" } */

static void
bar (void)
{
}

void *
foo (void)
{
  return &bar;
}

/* { dg-final { scan-assembler "mov\(l|q\)\[ \t\]*\\\$bar," } } */
/* { dg-final { scan-assembler-not "mov\(l|q\)\[ \t\]*bar@GOTPCREL" } } */
