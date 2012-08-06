/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */
/* { dg-final { scan-assembler "clr1 B100,#0" } } */

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
BitField B100 __attribute__ ((__BELOW100__));
unsigned char *p = (unsigned char *) &B100;

void
Do (void)
{
  B100.b0 = 0;
}

int
main (void)
{
  *p = 0xcb;
  Do ();
  return (*p == 0xca) ? 0 : 1;
}
