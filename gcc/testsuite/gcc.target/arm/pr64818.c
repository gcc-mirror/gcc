/* { dg-do compile } */
/* { dg-options "-O1" } */

char temp[16];
extern int foo1 (void);

void foo (void)
{
  int i;
  int len;

  while (1)
  {
    len = foo1 ();
    register int a asm ("r0") = 5;
    register char *b asm ("r1") = temp;
    register int c asm ("r2") = len;
    asm volatile ("mov %[r0], %[r0]\n  mov %[r1], %[r1]\n  mov %[r2], %[r2]\n"
		   : "+m"(*b)
		   : [r0]"r"(a), [r1]"r"(b), [r2]"r"(c));

    for (i = 0; i < len; i++)
    {
      if (temp[i] == 10)
      return;
    }
  }
}

/* { dg-final { scan-assembler "\[\\t \]+mov\ r1,\ r1" } } */
