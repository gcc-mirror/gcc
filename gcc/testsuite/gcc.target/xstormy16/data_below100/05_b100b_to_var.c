/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */

char acDummy[0xf0] __attribute__ ((__BELOW100__));
unsigned char B100 __attribute__ ((__BELOW100__)) = 0x34;
unsigned char *p = &B100;

unsigned char yData = 0x12;

void
Do (void)
{
  yData = B100;
}

int
main (void)
{
  Do ();
  return (yData == 0x34) ? 0 : 1;
}

/* { dg-final { scan-file "05_b100b_to_var.s" "mov.b r., *B100" } } */

