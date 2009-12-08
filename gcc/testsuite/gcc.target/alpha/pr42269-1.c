/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "addl" } } */

unsigned
parity (unsigned x)
{
  x ^= x >> 16;
  x ^= x >> 8;
  x ^= x >> 4;
  x &= 0xf;
  return (0x6996 >> x) & 1;
}
