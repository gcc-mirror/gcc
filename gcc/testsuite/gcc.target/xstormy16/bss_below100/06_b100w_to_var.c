/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */
/* { dg-final { scan-assembler "mov.w r.,B100" } } */

char acDummy[0xf0] __attribute__ ((__BELOW100__));
unsigned short B100 __attribute__ ((__BELOW100__));
unsigned short *p = &B100;

unsigned short wData = 0x1234;

void
Do (void)
{
  wData = B100;
}

int
main (void)
{
  *p = 0x3456;
  Do ();
  return (wData == 0x3456) ? 0 : 1;
}
