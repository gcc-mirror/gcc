/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */

char acDummy[0xf0] __attribute__ ((__BELOW100__));
unsigned short B100 __attribute__ ((__BELOW100__));
unsigned short *p = &B100;

void
Do (void)
{
  B100 |= 0x8000;
}

int
main (void)
{
  *p = 0x1234;
  Do ();
  return (*p == 0x9234) ? 0 : 1;
}

/* { dg-final { scan-file "14_set_b100w_bit_f.s" "set1 B100\\+1,#7" } } */

