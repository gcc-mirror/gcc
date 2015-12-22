/* { dg-do compile { target { *-*-linux* && ia32 } } } */
/* { dg-options "-O2 -fpic -fno-plt -mregparm=3" } */

extern void foo (int, int, int);
extern void bar (int, int, int) __attribute__((visibility("hidden")));

void
foo (int a, int b, int c)
{
  foo (a, b, c);
  bar (a, b, c);
  foo (a, b, c);
  bar (a, b, c);
}

/* { dg-final { scan-assembler "jmp\[ \t\]bar" } } */
