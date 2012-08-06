/* { dg-options { -nostartfiles below100.o -Tbelow100.ld -O2 } } */

#define SFR (*((volatile unsigned short*)0x7f14))
unsigned short *p = (unsigned short *) 0x7f14;

void
Do (void)
{
  SFR |= 0x0800;
}

int
main (void)
{
  *p = 0x1234;
  Do ();
  return (*p == 0x1a34) ? 0 : 1;
}

/* { dg-final { scan-file "14_set_sfrw_bit_b.s" "mov.w r.,32532" } } */

