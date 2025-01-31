/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fno-pic -fno-plt -mindirect-branch-register" } */

extern void bar (void) __attribute__((visibility("hidden")));

void
foo (void)
{
  bar ();
}

/* { dg-final { scan-assembler "jmp\[ \t\]*bar" } } */
