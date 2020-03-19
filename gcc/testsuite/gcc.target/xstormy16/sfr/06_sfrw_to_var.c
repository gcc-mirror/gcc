/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */
/* { dg-final { scan-assembler "mov.w r6,32532" } } */

#define SFR (*((volatile unsigned short*)0x7f14))
unsigned short *p = (unsigned short *) 0x7f14;

unsigned short wData = 0x9876;

void
Do (void)
{
  wData = SFR;
}

int
main (void)
{
  *p = 0x1234;
  Do ();
  return (wData == 0x1234) ? 0 : 1;
}
