/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fno-plt" } */

extern int bar (void);

int
foo (void)
{
  return bar () + 1;
}

/* { dg-final { scan-assembler "call\[ \t\]*.bar@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "call\[ \t\]*.bar@GOT\\(" { target ia32 } } } */
