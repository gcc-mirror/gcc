/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */
/* { dg-final { scan-assembler "mov.b r., *32532" } } */

#define SFR (*((volatile unsigned char*)0x7f14))
unsigned char *p = (unsigned char *) 0x7f14;

unsigned char yData = 0x12;

void
Do (void)
{
  yData = SFR;
}

int
main (void)
{
  *p = 0x34;
  Do ();
  return (yData == 0x34) ? 0 : 1;
}
