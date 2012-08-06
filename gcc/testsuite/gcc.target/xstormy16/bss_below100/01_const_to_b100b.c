/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */
/* { dg-final { scan-assembler "mov.b B100,#18" } } */

char acDummy[0xf0] __attribute__ ((__BELOW100__));
unsigned char B100 __attribute__ ((__BELOW100__));
unsigned char *p = &B100;

void
Do (void)
{
  B100 = 0x12;
}

int
main (void)
{
  *p = 0x34;
  Do ();
  return (*p == 0x12) ? 0 : 1;
}
