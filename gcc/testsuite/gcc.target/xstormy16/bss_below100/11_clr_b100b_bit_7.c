/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */
/* { dg-final { scan-assembler "clr1 B100,#7" } } */

char acDummy[0xf0] __attribute__ ((__BELOW100__));
unsigned char B100 __attribute__ ((__BELOW100__));
unsigned char *p = &B100;

void
Do (void)
{
  B100 &= ~0x80;
}

int
main (void)
{
  *p = 0xcb;
  Do ();
  return (*p == 0x4b) ? 0 : 1;
}
