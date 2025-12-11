/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "rotr\\.w" } } */

unsigned
t (unsigned a, unsigned b)
{
  return a << b | a >> (32 - b);
}
