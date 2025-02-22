/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mlsx" } */
/* { dg-final { scan-assembler "vsrari\\.w\t\\\$vr\[0-9\]+,\\\$vr\[0-9\]+,15" } } */

int x __attribute__ ((vector_size (16)));

void
f (void)
{
  x = (x + (1 << 14)) >> 15;
}
