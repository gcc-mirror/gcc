/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fno-plt" } */

extern void bar (void);

int
foo (void)
{
  bar ();
  return 0;
}

/* { dg-final { scan-assembler "call\[ \t\]*.bar@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "call\[ \t\]*.bar@GOT\\(" { target ia32 } } } */
