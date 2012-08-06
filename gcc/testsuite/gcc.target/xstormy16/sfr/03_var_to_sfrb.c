/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */
/* { dg-final { scan-assembler "mov.b 32532,r" } } */

#define SFR (*((volatile unsigned char*)0x7f14))
unsigned char *p = (unsigned char *) 0x7f14;

unsigned char yData = 0x12;

void
Do (void)
{
  SFR = yData;
}

int
main (void)
{
  *p = 0x34;
  Do ();
  return (*p == 0x12) ? 0 : 1;
}
