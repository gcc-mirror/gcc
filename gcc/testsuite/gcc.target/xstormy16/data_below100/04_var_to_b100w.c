/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */

char acDummy[0xf0] __attribute__ ((__BELOW100__));
unsigned short B100 __attribute__ ((__BELOW100__)) = 0x9876;
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
  Do ();
  return (*p == 0x1234) ? 0 : 1;
}

/* { dg-final { scan-file "04_var_to_b100w.s" "mov.w B100,r" } } */

