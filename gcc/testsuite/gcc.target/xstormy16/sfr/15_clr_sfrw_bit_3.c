/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */

#define SFR (*((volatile unsigned short*)0x7f14))
unsigned short *p = (unsigned short *) 0x7f14;

void
Do (void)
{
  SFR &= ~0x0008;
}

int
main (void)
{
  *p = 0xedcb;
  Do ();
  return (*p == 0xedc3) ? 0 : 1;
}

/* { dg-final { scan-file "15_clr_sfrw_bit_3.s" "mov.w r.,32532" } } */

