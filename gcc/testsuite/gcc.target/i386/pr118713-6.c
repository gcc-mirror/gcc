/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fno-pic -fno-plt -mindirect-branch-register" } */

extern void bar (void) __attribute__((visibility("hidden")));

int
foo (void)
{
  bar ();
  return 0;
}

/* { dg-final { scan-assembler "call\[ \t\]*bar" } } */
