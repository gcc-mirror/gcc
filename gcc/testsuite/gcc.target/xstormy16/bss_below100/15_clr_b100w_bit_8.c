/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */

char acDummy[0xf0] __attribute__ ((__BELOW100__));
unsigned short B100 __attribute__ ((__BELOW100__));
unsigned short *p = &B100;

void
Do (void)
{
  B100 &= ~0x0100;
}

int
main (void)
{
  *p = 0xedcb;
  Do ();
  return (*p == 0xeccb) ? 0 : 1;
}

/* { dg-final { scan-file "15_clr_b100w_bit_8.s" "clr1 B100\\+1,#0" } } */

