/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=future -mvsx" } */
/* { dg-require-effective-target powerpc_future_compile_ok } */

#include <altivec.h>

vector unsigned short test_uncompresshn(vector unsigned char a,
                                        vector unsigned int b)
{
  return vec_uncompresshn(a, b);
}

vector unsigned int test_uncompresshb(vector unsigned short a,
                                      vector unsigned short b)
{
  return vec_uncompresshb(a, b);
}

vector unsigned long long test_uncompresshh(vector unsigned int a,
                                            vector unsigned char b)
{
  return vec_uncompresshh(a, b);
}

vector unsigned short test_uncompressln(vector unsigned char a,
                                        vector unsigned int b)
{
  return vec_uncompressln(a, b);
}

vector unsigned int test_uncompresslb(vector unsigned short a,
                                      vector unsigned short b)
{
  return vec_uncompresslb(a, b);
}

vector unsigned long long test_uncompresslh(vector unsigned int a,
                                            vector unsigned char b)
{
  return vec_uncompresslh(a, b);
}

/* BE: direct instructions, no splats or reverse. */

/* { dg-final { scan-assembler-not {vsplt[whb]} { target { be } } } } */
/* { dg-final { scan-assembler-not {xxspltib} { target { be } } } } */
/* { dg-final { scan-assembler-not {xxbr[wh]\M} { target { be } } } } */
/* { dg-final { scan-assembler-not {vrlb} { target { be } } } } */

/* LE: splats and reverse-byte + rotate must appear. */

/* { dg-final { scan-assembler-times {vsplt[whb]} 6 { target { le } } } } */

/* { dg-final { scan-assembler-times "xxspltib" 6 { target { le } } } } */

/* { dg-final { scan-assembler-times {xxbr[wh]} 4 { target { le } } } } */

/* { dg-final { scan-assembler-times "vrlb" 6 { target { le } } } } */

/* { dg-final { scan-assembler-times {vucmpr[lh][nbh]} 6 } } */

