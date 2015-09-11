/* { dg-do run { target aarch64*-*-* } } */
/* { dg-options "-O2 --save-temps -fno-inline" } */
/* { dg-require-effective-target aarch64_big_endian } */

extern void abort (void);

typedef struct bitfield
{
  unsigned short eight1: 8;
  unsigned short four: 4;
  unsigned short eight2: 8;
  unsigned short seven: 7;
  unsigned int sixteen: 16;
  unsigned short eight3: 8;
  unsigned short eight4: 8;
} bitfield;

bitfield
bfxil (bitfield a)
{
  /* { dg-final { scan-assembler "bfxil\tx\[0-9\]+, x\[0-9\]+, 40, 8" } } */
  a.eight4 = a.eight2;
  return a;
}

int
main (void)
{
  static bitfield a;
  bitfield b;

  a.eight4 = 9;
  a.eight2 = 57;
  b = bfxil (a);

  if (b.eight4 != a.eight2)
    abort ();

  return 0;
}

