/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fno-plt" } */

extern char* bar (int);
extern char* arr[32];

void
foo (void)
{
  int i;

  for (i = 0; i < 32; i++)
    arr[i] = bar (128);
}

/* { dg-final { scan-assembler "call\[ \t\]*.bar@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "call\[ \t\]*.bar@GOT\\(" { target ia32 } } } */
/* { dg-final { scan-assembler-not "mov(l|q)\[ \t\]*.bar@GOTPCREL" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "movl\[ \t\]*.bar@GOT\\(" { target ia32 } } } */
/* { dg-final { scan-assembler-not "call\[ \t\]*.bar@PLT" } } */
