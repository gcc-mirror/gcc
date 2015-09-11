/* { dg-do run } */
/* { dg-options "-O2 -fno-inline --save-temps" } */

extern void abort (void);

typedef long long s64int;
typedef int s32int;
typedef unsigned long long u64int;
typedef unsigned int u32int;

s64int
anddi_di_notdi (s64int a, s64int b)
{
  return (a & ~b);
}

s64int
anddi_di_notzesidi (s64int a, u32int b)
{
  return (a & ~(u64int) b);
}

s64int
anddi_notdi_zesidi (s64int a, u32int b)
{
  return (~a & (u64int) b);
}

s64int
anddi_di_notsesidi (s64int a, s32int b)
{
  return (a & ~(s64int) b);
}

int main ()
{
  s64int a64 = 0xdeadbeef0000ffffll;
  s64int b64 = 0x000000005f470112ll;
  s64int c64 = 0xdeadbeef300f0000ll;

  u32int c32 = 0x01124f4f;
  s32int d32 = 0xabbaface;

  s64int z = anddi_di_notdi (c64, b64);
  if (z != 0xdeadbeef20080000ll)
    abort ();

  z = anddi_di_notzesidi (a64, c32);
  if (z != 0xdeadbeef0000b0b0ll)
    abort ();

  z = anddi_notdi_zesidi (c64, c32);
  if (z != 0x0000000001104f4fll)
    abort ();

  z = anddi_di_notsesidi (a64, d32);
  if (z != 0x0000000000000531ll)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "bics\t" 6 { target arm_thumb1 } } } */
/* { dg-final { scan-assembler-times "bic\t" 6 { target { ! arm_thumb1 } } } } */

