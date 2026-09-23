/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=future -mno-vsx" } */
/* { dg-require-effective-target powerpc_future_compile_ok } */

#include <altivec.h>

vector unsigned short
test_uncompresshn (vector unsigned char a,
                   vector unsigned int b)
{
  return vec_uncompresshn (a, b);
}

vector unsigned int
test_uncompresshb (vector unsigned short a,
                   vector unsigned short b)
{
  return vec_uncompresshb (a, b);
}

vector unsigned short
test_uncompressln (vector unsigned char a,
                   vector unsigned int b)
{
  return vec_uncompressln (a, b);
}

vector unsigned int
test_uncompresslb (vector unsigned short a,
                   vector unsigned short b)
{
  return vec_uncompresslb (a, b);
}

/* { dg-final { scan-assembler-not "vxor" { target { be } } } } */
/* { dg-final { scan-assembler-not "vinsw" { target { be } } } } */
/* { dg-final { scan-assembler-not "vperm" { target { be } } } } */


/* { dg-final { scan-assembler-times "vxor" 4 { target { le } } } } */
/* { dg-final { scan-assembler-times "vinsw" 4 { target { le } } } } */
/* { dg-final { scan-assembler-times "vperm" 4 { target { le } } } } */
/* { dg-final { scan-assembler-times "vrlb" 4 { target { le } } } } */
/* { dg-final { scan-assembler-times "vspltisb" 4 { target { le } } } } */

/* Final uncompress instructions.  */

/* { dg-final { scan-assembler-times {vucmpr[lh][nb]} 4 } } */

