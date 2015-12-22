/* { dg-do compile { target { *-*-linux* && ia32 } } } */
/* { dg-options "-O2 -fpic -fno-plt -mregparm=3" } */

extern void bar (int, int);

void
foo (int a, int b)
{
  bar (a, b);
  bar (a, b);
}

/* { dg-final { scan-assembler "jmp\[ \t\]*.bar@GOT\\(%e(a|c|d)x\\)" } } */
