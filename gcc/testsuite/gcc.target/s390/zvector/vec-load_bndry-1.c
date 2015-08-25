/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O0 -mzarch -march=z13 -mzvector" } */

#include <vecintrin.h>

signed char
foo64 (signed char *p)
{
  return vec_load_bndry (p, 64)[0];
  /* { dg-final { scan-assembler-times "\tvlbb\t%v..?,0\\(%r..?\\),0" 1 } } */
}

signed char
foo128 (signed char *p)
{
  return
    vec_load_bndry (p, 128)[0]
    + vec_load_bndry (p + 16, 128)[0];
  /* { dg-final { scan-assembler-times "\tvlbb\t%v..?,0\\(%r..?\\),1" 2 } } */
}

signed char
foo256 (signed char *p)
{
  return
    vec_load_bndry (p, 256)[0]
    + vec_load_bndry (p + 16, 256)[0]
    + vec_load_bndry (p + 32, 256)[0];
  /* { dg-final { scan-assembler-times "\tvlbb\t%v..?,0\\(%r..?\\),2" 3 } } */
}

signed char
foo512 (signed char *p)
{
  return
    vec_load_bndry (p, 512)[0]
    + vec_load_bndry (p + 16, 512)[0]
    + vec_load_bndry (p + 32, 512)[0]
    + vec_load_bndry (p + 48, 512)[0];
  /* { dg-final { scan-assembler-times "\tvlbb\t%v..?,0\\(%r..?\\),3" 4 } } */
}

signed char
foo1024 (signed char *p)
{
  return
    vec_load_bndry (p, 1024)[0]
    + vec_load_bndry (p + 16, 1024)[0]
    + vec_load_bndry (p + 32, 1024)[0]
    + vec_load_bndry (p + 48, 1024)[0]
    + vec_load_bndry (p + 64, 1024)[0];
  /* { dg-final { scan-assembler-times "\tvlbb\t%v..?,0\\(%r..?\\),4" 5 } } */
}

signed char
foo2048 (signed char *p)
{
  return
    vec_load_bndry (p, 2048)[0]
    + vec_load_bndry (p + 16, 2048)[0]
    + vec_load_bndry (p + 32, 2048)[0]
    + vec_load_bndry (p + 48, 2048)[0]
    + vec_load_bndry (p + 64, 2048)[0]
    + vec_load_bndry (p + 80, 2048)[0];
  /* { dg-final { scan-assembler-times "\tvlbb\t%v..?,0\\(%r..?\\),5" 6 } } */
}

signed char
foo4096 (signed char *p)
{
  return
    vec_load_bndry (p, 4096)[0]
    + vec_load_bndry (p + 16, 4096)[0]
    + vec_load_bndry (p + 32, 4096)[0]
    + vec_load_bndry (p + 48, 4096)[0]
    + vec_load_bndry (p + 64, 4096)[0]
    + vec_load_bndry (p + 80, 4096)[0]
    + vec_load_bndry (p + 96, 4096)[0];
  /* { dg-final { scan-assembler-times "\tvlbb\t%v..?,0\\(%r..?\\),6" 7 } } */
}
