/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fno-pic -fno-plt" } */

extern int bar (void) __attribute__((visibility("hidden")));

int
foo (void)
{
  return bar () + 1;
}

/* { dg-final { scan-assembler-not "call\[ \t\]*.bar@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "call\[ \t\]*.bar@GOT" { target ia32 } } } */
