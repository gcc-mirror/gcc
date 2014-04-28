/* { dg-do run } */
/* { dg-options "-O2 -fno-inline --save-temps" } */

extern void abort (void);

typedef long long s64int;
typedef int s32int;
typedef unsigned long long u64int;
typedef unsigned int u32int;

s64int
iordi_di_notdi (s64int a, s64int b)
{
  return (a | ~b);
}

s64int
iordi_di_notzesidi (s64int a, u32int b)
{
  return (a | ~(u64int) b);
}

s64int
iordi_notdi_zesidi (s64int a, u32int b)
{
  return (~a | (u64int) b);
}

s64int
iordi_di_notsesidi (s64int a, s32int b)
{
  return (a | ~(s64int) b);
}

int main ()
{
  s64int a64 = 0xdeadbeef00000000ll;
  s64int b64 = 0x000000004f4f0112ll;
  s64int c64 = 0xdeadbeef000f0000ll;

  u32int c32 = 0x01124f4f;
  s32int d32 = 0xabbaface;

  s64int z = iordi_di_notdi (a64, b64);
  if (z != 0xffffffffb0b0feedll)
    abort ();

  z = iordi_di_notzesidi (a64, c32);
  if (z != 0xfffffffffeedb0b0ll)
    abort ();

  z = iordi_notdi_zesidi (c64, c32);
  if (z != 0x21524110fff2ffffll)
    abort ();

  z = iordi_di_notsesidi (a64, d32);
  if (z != 0xdeadbeef54450531ll)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "orn\t" 6 { target arm_thumb2 } } } */

/* { dg-final { cleanup-saved-temps } } */
