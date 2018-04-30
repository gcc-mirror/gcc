/* { dg-do compile { target { powerpc-*-linux* && ilp32 } } } */
/* { dg-options "-O2 -fpic -fno-reorder-blocks" } */

/* Analog of pr79439-1.c for 32-bit Linux.  */

int f (void);

void
g (void)
{
}

int
rec (int a)
{
  int ret = 0;
  if (a > 10 && f ())
    ret += rec (a - 1);
  g ();
  return a + ret;
}

/* { dg-final { scan-assembler-times {\mbl f@plt\M}   1 } } */
/* { dg-final { scan-assembler-times {\mbl g@plt\M}   1 } } */
/* { dg-final { scan-assembler-times {\mbl rec@plt\M} 0 } } */
