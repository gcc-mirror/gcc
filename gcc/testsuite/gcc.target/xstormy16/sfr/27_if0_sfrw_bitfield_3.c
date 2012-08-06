/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */
/* { dg-final { scan-assembler "bp 32532,#3" } } */

typedef struct
{
  unsigned short b0:1;
  unsigned short b1:1;
  unsigned short b2:1;
  unsigned short b3:1;
  unsigned short b4:1;
  unsigned short b5:1;
  unsigned short b6:1;
  unsigned short b7:1;
  unsigned short b8:1;
  unsigned short b9:1;
  unsigned short b10:1;
  unsigned short b11:1;
  unsigned short b12:1;
  unsigned short b13:1;
  unsigned short b14:1;
  unsigned short b15:1;
} BitField;

#define SFRA (*((volatile BitField*)0x7f14))
unsigned short *pA = (unsigned short *) 0x7f14;
#define SFRB (*((volatile BitField*)0x7f10))
unsigned short *pB = (unsigned short *) 0x7f10;

char *
Do (void)
{
  if (!SFRA.b3)
    {
      if (!SFRB.b3)
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
  *pA = 0x1234;
  *pB = 0xedcb;
  return Do ()[0] == 'F';
}
