/* { dg-do compile { target lp64 } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Test to make sure VEXTU{B,H,W}{L,R}X is generated for various vector extract
   operations for ISA 3.0 (-mcpu=power9).  In addition, make sure that neither
   of the old methods of doing vector extracts are done either by
   explict stores to the stack or by using direct move instructions.  */

#include <altivec.h>

int
extract_int_0 (vector int a)
{
  int c = 0;
  int b = vec_extract (a, c);
  return b;
}

int
extract_int_3 (vector int a)
{
  int c = 3;
  int b = vec_extract (a, c);
  return b;
}

unsigned int
extract_uint_0 (vector unsigned int a)
{
  int c = 0;
  unsigned int b = vec_extract (a, c);
  return b;
}

unsigned int
extract_uint_3 (vector unsigned int a)
{
  int c = 3;
  unsigned int b = vec_extract (a, c);
  return b;
}

short
extract_short_0 (vector short a)
{
  int c = 0;
  short b = vec_extract (a, c);
  return b;
}

short
extract_short_7 (vector short a)
{
  int c = 7;
  short b = vec_extract (a, c);
  return b;
}

unsigned short
extract_ushort_0 (vector unsigned short a)
{
  int c = 0;
  unsigned short b = vec_extract (a, c);
  return b;
}

unsigned short
extract_ushort_7 (vector unsigned short a)
{
  int c = 7;
  unsigned short b = vec_extract (a, c);
  return b;
}

signed char
extract_schar_0 (vector signed char a)
{
  int c = 0;
  signed char b = vec_extract (a, c);
  return b;
}

signed char
extract_schar_15 (vector signed char a)
{
  int c = 15;
  signed char b = vec_extract (a, c);
  return b;
}

unsigned char
extract_uchar_0 (vector unsigned char a)
{
  int c = 0;
  unsigned char b = vec_extract (a, c);
  return b;
}

unsigned char
extract_uchar_15 (vector unsigned char a)
{
  int c = 15;
  signed char b = vec_extract (a, c);
  return b;
}

unsigned char
extract_bool_char_0 (vector bool char a)
{
  int c = 0;
  unsigned char b = vec_extract (a, c);
  return b;
}

unsigned int
extract_bool_int_0 (vector bool int a)
{
  int c = 0;
  unsigned int b = vec_extract (a, c);
  return b;
}

unsigned short int
extract_bool_short_int_0 (vector bool short int a)
{
  int c = 0;
  unsigned short int b = vec_extract (a, c);
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
