/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */
/* { dg-final { scan-assembler "mov.w B100,#4660" } } */

char acDummy[0xf0] __attribute__ ((__BELOW100__));
unsigned short B100 __attribute__ ((__BELOW100__));
unsigned short *p = &B100;

void
Do (void)
{
  B100 = 0x1234;
}

int
main (void)
{
  *p = 0x9876;
  Do ();
  return (*p == 0x1234) ? 0 : 1;
}
