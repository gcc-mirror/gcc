/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -fno-pic -fno-plt" } */

extern void bar (void);
extern void *p;

void
foo (void)
{
  p = &bar;
}

/* { dg-final { scan-assembler "mov\(l|q\)\[ \t\]*bar@GOTPCREL" } } */
/* { dg-final { scan-assembler-not "mov\(l|q\)\[ \t\]*\\\$bar," } } */
