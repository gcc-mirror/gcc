/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */
/* { dg-final { scan-assembler "mov.w B100,r" } } */

char acDummy[0xf0] __attribute__ ((__BELOW100__));
unsigned short B100 __attribute__ ((__BELOW100__));
unsigned short *p = &B100;

unsigned short wData = 0x1234;

void
Do (void)
{
  B100 = wData;
}

int
main (void)
{
  *p = 0x9876;
  Do ();
  return (*p == 0x1234) ? 0 : 1;
}
