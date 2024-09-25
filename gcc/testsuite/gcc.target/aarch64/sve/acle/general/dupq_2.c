/* { dg-do compile } */
/* { dg-options "-O2 -mbig-endian" } */

/* To avoid needing big-endian header files.  */
#pragma GCC aarch64 "arm_sve.h"

svint32_t
dupq (int x)
{
  return svdupq_s32 (x, 1, 2, 3);
}

/* { dg-final { scan-assembler {\tindex\tz[0-9]+\.s, #3, #-1} } } */
/* { dg-final { scan-assembler {\tins\tv[0-9]+\.s\[0\], w0\n} } } */
/* { dg-final { scan-assembler {\tdup\tz[0-9]+\.q, z[0-9]+\.q\[0\]\n} } } */
