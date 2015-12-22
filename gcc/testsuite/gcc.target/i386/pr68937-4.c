/* { dg-do compile { target { *-*-linux* && ia32 } } } */
/* { dg-options "-O2 -fpic -fno-plt -mregparm=3" } */

extern int bar (int, int);

int
foo (int a, int b)
{
  (void) bar (a, b);
  return bar (a, b);
}

/* { dg-final { scan-assembler "jmp\[ \t\]*.bar@GOT\\(%e(a|c|d)x\\)" } } */
