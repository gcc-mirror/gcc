/* { dg-do run { target aarch64*-*-* } } */
/* { dg-options "-O2 --save-temps -fno-inline" } */
/* { dg-require-effective-target aarch64_little_endian } */

extern void abort (void);

typedef struct bitfield
{
  unsigned short eight: 8;
  unsigned short four: 4;
  unsigned short five: 5;
  unsigned short seven: 7;
  unsigned int sixteen: 16;
} bitfield;

bitfield
bfi1 (bitfield a)
{
  /* { dg-final { scan-assembler "bfi\tx\[0-9\]+, x\[0-9\]+, 0, 8" } } */
  a.eight = 3;
  return a;
}

bitfield
bfi2 (bitfield a)
{
  /* { dg-final { scan-assembler "bfi\tx\[0-9\]+, x\[0-9\]+, 16, 5" } } */
  a.five = 7;
  return a;
}

bitfield
movk (bitfield a)
{
  /* { dg-final { scan-assembler "movk\tx\[0-9\]+, 0x1d6b, lsl 32" } } */
  a.sixteen = 7531;
  return a;
}

bitfield
set1 (bitfield a)
{
  /* { dg-final { scan-assembler "orr\tx\[0-9\]+, x\[0-9\]+, 2031616" } } */
  a.five = 0x1f;
  return a;
}

bitfield
set0 (bitfield a)
{
  /* { dg-final { scan-assembler "and\tx\[0-9\]+, x\[0-9\]+, -2031617" } } */
  a.five = 0;
  return a;
}


int
main (int argc, char** argv)
{
  static bitfield a;
  bitfield b = bfi1 (a);
  bitfield c = bfi2 (b);
  bitfield d = movk (c);

  if (d.eight != 3)
    abort ();

  if (d.five != 7)
    abort ();

  if (d.sixteen != 7531)
    abort ();

  d = set1 (d);
  if (d.five != 0x1f)
    abort ();

  d = set0 (d);
  if (d.five != 0)
    abort ();

  return 0;
}

