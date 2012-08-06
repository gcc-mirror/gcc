/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */
/* { dg-final { scan-assembler "clr1 B100,#0" } } */

char acDummy[0xf0] __attribute__ ((__BELOW100__));
unsigned char B100 __attribute__ ((__BELOW100__)) = 0xcb;
unsigned char *p = &B100;

void
Do (void)
{
  B100 &= ~0x01;
}

int
main (void)
{
  Do ();
  return (*p == 0xca) ? 0 : 1;
}
