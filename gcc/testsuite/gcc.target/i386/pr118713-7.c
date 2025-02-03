/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fno-pic -fno-plt -mindirect-branch-register" } */

extern int bar (void) __attribute__((visibility("hidden")));

int
foo (void)
{
  return bar ();
}

/* { dg-final { scan-assembler "jmp\[ \t\]*bar" } } */
