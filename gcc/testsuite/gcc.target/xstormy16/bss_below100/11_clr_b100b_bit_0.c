/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */

char acDummy[0xf0] __attribute__ ((__BELOW100__));
unsigned char B100 __attribute__ ((__BELOW100__));
unsigned char *p = &B100;

void
Do (void)
{
  B100 &= ~0x01;
}

int
main (void)
{
  *p = 0xcb;
  Do ();
  return (*p == 0xca) ? 0 : 1;
}

/* { dg-final { scan-file "11_clr_b100b_bit_0.s" "clr1 B100,#0" } } */

