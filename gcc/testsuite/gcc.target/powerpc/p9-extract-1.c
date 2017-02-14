/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9 -O2" } */

/* Test to make sure VEXTU{B,H,W}{L,R}X is generated for various vector extract
   operations for ISA 3.0 (-mcpu=power9).  In addition, make sure that neither
   of the the the old methods of doing vector extracts are done either by
   explict stores to the stack or by using direct move instructions.  */

#include <altivec.h>

int
extract_int_0 (vector int a)
{
  int b = vec_extract (a, 0);
  return b;
}

int
extract_int_3 (vector int a)
{
  int b = vec_extract (a, 3);
  return b;
}

unsigned int
extract_uint_0 (vector unsigned int a)
{
  unsigned int b = vec_extract (a, 0);
  return b;
}

unsigned int
extract_uint_3 (vector unsigned int a)
{
  unsigned int b = vec_extract (a, 3);
  return b;
}

short
extract_short_0 (vector short a)
{
  short b = vec_extract (a, 0);
  return b;
}

short
extract_short_7 (vector short a)
{
  short b = vec_extract (a, 7);
  return b;
}

unsigned short
extract_ushort_0 (vector unsigned short a)
{
  unsigned short b = vec_extract (a, 0);
  return b;
}

unsigned short
extract_ushort_7 (vector unsigned short a)
{
  unsigned short b = vec_extract (a, 7);
  return b;
}

signed char
extract_schar_0 (vector signed char a)
{
  signed char b = vec_extract (a, 0);
  return b;
}

signed char
extract_schar_15 (vector signed char a)
{
  signed char b = vec_extract (a, 15);
  return b;
}

unsigned char
extract_uchar_0 (vector unsigned char a)
{
  unsigned char b = vec_extract (a, 0);
  return b;
}

unsigned char
extract_uchar_15 (vector unsigned char a)
{
  signed char b = vec_extract (a, 15);
  return b;
}

/* { dg-final { scan-assembler     "vextub\[lr\]x " } } */
/* { dg-final { scan-assembler     "vextuh\[lr\]x " } } */
/* { dg-final { scan-assembler     "vextuw\[lr\]x " } } */
/* { dg-final { scan-assembler     "extsb "         } } */
/* { dg-final { scan-assembler     "extsh "         } } */
/* { dg-final { scan-assembler     "extsw "         } } */
/* { dg-final { scan-assembler-not "m\[ft\]vsr"     } } */
/* { dg-final { scan-assembler-not "stxvd2x "       } } */
/* { dg-final { scan-assembler-not "stxv "          } } */
/* { dg-final { scan-assembler-not "lwa "           } } */
/* { dg-final { scan-assembler-not "lwz "           } } */
/* { dg-final { scan-assembler-not "lha "           } } */
/* { dg-final { scan-assembler-not "lhz "           } } */
