/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */
/* { dg-final { scan-assembler "mov.w 32532,r" } } */

#define SFR (*((volatile unsigned short*)0x7f14))
unsigned short *p = (unsigned short *) 0x7f14;

unsigned short wData = 0x1234;

void
Do (void)
{
  SFR = wData;
}

int
main (void)
{
  *p = 0x9876;
  Do ();
  return (*p == 0x1234) ? 0 : 1;
}
