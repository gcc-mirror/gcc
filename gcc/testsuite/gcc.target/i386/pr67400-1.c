/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -fno-pic -fno-plt" } */

extern void bar (void);

void *
foo (void)
{
  return &bar;
}

/* { dg-final { scan-assembler "mov\(l|q\)\[ \t\]*bar@GOTPCREL" } } */
/* { dg-final { scan-assembler-not "\(mov|lea\)\(l|q\)\[ \t\]*\(\\\$|\)bar," } } */
