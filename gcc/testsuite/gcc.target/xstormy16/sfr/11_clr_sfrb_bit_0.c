/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */
/* { dg-final { scan-assembler "clr1 32532,#0" } } */

#define SFR (*((volatile unsigned char*)0x7f14))
unsigned char *p = (unsigned char *) 0x7f14;

void
Do (void)
{
  SFR &= ~0x01;
}

int
main (void)
{
  *p = 0xcb;
  Do ();
  return (*p == 0xca) ? 0 : 1;
}
