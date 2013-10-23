/* { dg-do compile { target sse2 } } */
/* { dg-options "-O2 -ftree-vectorize -fno-common -msse2" } */

unsigned short b[1024] = { 0 };
int a[1024] = { 0 };

int
f1 (int x)
{
  int i;
  for (i = 0; i < 1024; i++)
    a[i] = (b[i] + 7) / 15;
}

int
f2 (int x)
{
  int i;
  for (i = 0; i < 1024; i++)
    a[i] = (b[i] + 7) % 15;
}

int
f3 (int x)
{
  int i;
  for (i = 0; i < 1024; i++)
    a[i] = (b[i] - 66000) / 15;
}

int
f4 (int x)
{
  int i;
  for (i = 0; i < 1024; i++)
    a[i] = (b[i] - 66000) % 15;
}

/* In f1 and f2, VRP can prove the first operand of division or modulo
   is always non-negative, so there is no need to do >> 31 shift
   etc. to check if it is.  And in f3 and f4, VRP can prove it is always
   negative.  */
/* { dg-final { scan-assembler-not "psrad\[^\n\r\]*\\\$31" } } */
