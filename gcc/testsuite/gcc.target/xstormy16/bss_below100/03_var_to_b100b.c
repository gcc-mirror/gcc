/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */

char acDummy[0xf0] __attribute__ ((__BELOW100__));
unsigned char B100 __attribute__ ((__BELOW100__));
unsigned char *p = &B100;

unsigned char yData = 0x12;

void
Do (void)
{
  B100 = yData;
}

int
main (void)
{
  *p = 0x34;
  Do ();
  return (*p == 0x12) ? 0 : 1;
}

/* { dg-final { scan-file "03_var_to_b100b.s" "mov.b B100,r" } } */

