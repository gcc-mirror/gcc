/* Copyright (C) 2003  Free Software Foundation.
   Check that size-optimizations for move insns (specifically peephole
   optimizations) aren't applied to volatile objects in the CRIS port.
   Origin: Hans-Peter Nilsson.  */
/* { dg-do compile { target cris-*-* } } */
/* { dg-final { scan-assembler-not {movu\...\[} } } */
/* { dg-final { scan-assembler-not {move\.[^d].\[} } } */
/* { dg-final { scan-assembler-not {and\.[^d].\[} } } */
/* { dg-final { scan-assembler-not {or\.[^d].\[} } } */

static const unsigned long c = 0x0000FF00;
unsigned long
a1 (void)
{
  unsigned long m;
  m = *(volatile unsigned long*) 0xb00000c8;
  m &= c;
  return m;
}
extern volatile unsigned long xx;
unsigned long
a2 (void)
{
  unsigned long m;
  m = xx;
  m &= c;
  return m;
}
extern volatile unsigned long yy[];
unsigned long
a3 (void)
{
  unsigned long m;
  m = yy[3];
  m &= 0xfe00;
  return m;
}
unsigned long
ac1 (void)
{
  unsigned long m;
  m = *(volatile unsigned long*) 0xb00000c8;
  m &= 0xfe00;
  return m;
}
extern volatile unsigned long xx;
unsigned long
ac2 (void)
{
  unsigned long m;
  m = xx;
  m &= 0xfe00;
  return m;
}
extern volatile unsigned long yy[];
unsigned long
ac3 (void)
{
  unsigned long m;
  m = yy[3];
  m &= 0xfe00;
  return m;
}
extern volatile unsigned long yy[];
unsigned long
oc3 (void)
{
  unsigned long m;
  m = yy[3];
  m |= ~0xf;
  return m;
}
