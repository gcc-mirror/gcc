/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fno-pic -fno-plt -mindirect-branch-register" } */

extern int bar (void) __attribute__((visibility("hidden")));

int
foo (void)
{
  return bar () + 1;
}

/* { dg-final { scan-assembler "call\[ \t\]*bar" } } */
