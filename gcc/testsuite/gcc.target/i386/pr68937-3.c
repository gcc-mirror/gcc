/* { dg-do compile { target { *-*-linux* && ia32 } } } */
/* { dg-options "-O2 -fpic -fno-plt -mregparm=3" } */

extern void bar (int, int, int);

void
foo (int a, int b, int c)
{
  bar (a, b, c);
  bar (a, b, c);
}

/* { dg-final { scan-assembler-not "jmp\[ \t\]*.bar@GOT" } } */
