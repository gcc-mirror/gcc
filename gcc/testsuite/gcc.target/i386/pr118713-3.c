/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fno-plt -mindirect-branch-register" } */

extern int bar (void);

int
foo (void)
{
  return bar ();
}

/* { dg-final { scan-assembler "mov\(l|q\)\[ \t\]*bar@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movl\[ \t\]*bar@GOT\\(" { target ia32 } } } */
/* { dg-final { scan-assembler "jmp\[ \t\]*\\*%" } } */
