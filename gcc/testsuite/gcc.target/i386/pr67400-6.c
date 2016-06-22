/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -fno-pic -fno-plt" } */

extern int bar (void);

int
check (void *p)
{
  return p != &bar;
}

/* { dg-final { scan-assembler "cmp\(l|q\)\[ \t\]*.*bar@GOTPCREL" } } */
/* { dg-final { scan-assembler-not "mov\(l|q\)\[ \t\]*\\\$bar," } } */
