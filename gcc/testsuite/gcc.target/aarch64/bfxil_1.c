/* { dg-do run { target aarch64*-*-* } } */
/* { dg-options "-O2 --save-temps -fno-inline" } */
/* { dg-require-effective-target aarch64_little_endian } */

extern void abort (void);

typedef struct bitfield
{
  unsigned short eight1: 8;
  unsigned short four: 4;
  unsigned short eight2: 8;
  unsigned short seven: 7;
  unsigned int sixteen: 16;
} bitfield;

bitfield
bfxil (bitfield a)
{
  /* { dg-final { scan-assembler "bfxil\tx\[0-9\]+, x\[0-9\]+, 16, 8" } } */
  a.eight1 = a.eight2;
  return a;
}

int
main (void)
{
  static bitfield a;
  bitfield b;

  a.eight1 = 9;
  a.eight2 = 57;
  b = bfxil (a);

  if (b.eight1 != a.eight2)
    abort ();

  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
