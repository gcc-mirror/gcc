/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */
/* { dg-final { scan-assembler "b\[np\] B100A,#0," } } */
/* { dg-final { scan-assembler "b\[np\] B100B,#0," } } */

char acDummy[0xf0] __attribute__ ((__BELOW100__));
unsigned short B100A __attribute__ ((__BELOW100__)) = 0x1234;
unsigned short *pA = &B100A;
unsigned short B100B __attribute__ ((__BELOW100__)) = 0xedcb;
unsigned short *pB = &B100B;

char *
Do (void)
{
  if (!(B100A & 0x0001))
    {
      if (!(B100B & 0x0001))
	return "Fail";
      else
	return "Success";
    }
  else
    return "Fail";
}

int
main (void)
{
  return Do ()[0] == 'F';
}
