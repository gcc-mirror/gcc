/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */
/* { dg-final { scan-assembler "b\[np\] B100A,#3," } } */
/* { dg-final { scan-assembler "b\[np\] B100B,#3," } } */

typedef struct
{
  unsigned char b0:1;
  unsigned char b1:1;
  unsigned char b2:1;
  unsigned char b3:1;
  unsigned char b4:1;
  unsigned char b5:1;
  unsigned char b6:1;
  unsigned char b7:1;
} BitField;

char acDummy[0xf0] __attribute__ ((__BELOW100__));
BitField B100A __attribute__ ((__BELOW100__));
unsigned char *pA = (unsigned char *) &B100A;
BitField B100B __attribute__ ((__BELOW100__));
unsigned char *pB = (unsigned char *) &B100B;

char *
Do (void)
{
  if (B100A.b3)
    {
      if (B100B.b3)
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
  *pA = 0xcb;
  *pB = 0x34;
  return Do ()[0] == 'F';
}
