/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target aarch64_little_endian } */

#include <arm_sve.h>

svint32_t
dupq (int x)
{
  return svdupq_s32 (x, 1, 2, 3);
}

/* { dg-final { scan-assembler {\tldr\tq[0-9]+,} } } */
/* { dg-final { scan-assembler {\tins\tv[0-9]+\.s\[0\], w0\n} } } */
/* { dg-final { scan-assembler {\tdup\tz[0-9]+\.q, z[0-9]+\.q\[0\]\n} } } */
/* { dg-final { scan-assembler {\t\.word\t1\n\t\.word\t2\n\t\.word\t3\n} } } */
