/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */
/* { dg-final { scan-assembler "mov.w r.,32532" } } */

#define SFR (*((volatile unsigned short*)0x7f14))
unsigned short *p = (unsigned short *) 0x7f14;

void
Do (void)
{
  SFR &= ~0x0001;
}

int
main (void)
{
  *p = 0xedcb;
  Do ();
  return (*p == 0xedca) ? 0 : 1;
}
