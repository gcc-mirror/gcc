/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "rotr\\.w\t\\\$r4,\\\$r4,\\\$r5" } } */
/* { dg-final { scan-assembler "rotri\\.w\t\\\$r4,\\\$r4,5" } } */
/* { dg-final { scan-assembler-not "slli\\.w" } } */

unsigned
rotr (unsigned a, unsigned b)
{
  return a >> b | a << 32 - b;
}

unsigned
rotri (unsigned a)
{
  return a >> 5 | a << 27;
}
