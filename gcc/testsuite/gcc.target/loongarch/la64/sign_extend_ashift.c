/* { dg-do compile { target { loongarch64*-*-* } } } */
/* { dg-options "-O3" } */
/* { dg-final { scan-assembler "slli\\.w" } } */
/* { dg-final { scan-assembler-not "slli\\.d" } } */
/* { dg-final { scan-assembler-not "ext\\.w\\.b" } } */

unsigned int
test (unsigned int id)
{
  return id << 24;
}
