/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

unsigned char
add_unsigned_char_0 (vector unsigned char *p)
{
  return vec_extract (*p, 0) + 1;
}

unsigned char
add_unsigned_char_1 (vector unsigned char *p)
{
  return vec_extract (*p, 1) + 1;
}

unsigned char
add_unsigned_char_2 (vector unsigned char *p)
{
  return vec_extract (*p, 2) + 1;
}

unsigned char
add_unsigned_char_3 (vector unsigned char *p)
{
  return vec_extract (*p, 3) + 1;
}

unsigned char
add_unsigned_char_4 (vector unsigned char *p)
{
  return vec_extract (*p, 4) + 1;
}

unsigned char
add_unsigned_char_5 (vector unsigned char *p)
{
  return vec_extract (*p, 5) + 1;
}

unsigned char
add_unsigned_char_6 (vector unsigned char *p)
{
  return vec_extract (*p, 6) + 1;
}

unsigned char
add_unsigned_char_7 (vector unsigned char *p)
{
  return vec_extract (*p, 7) + 1;
}

unsigned char
add_unsigned_char_n (vector unsigned char *p, int n)
{
  return vec_extract (*p, n) + 1;
}

/* { dg-final { scan-assembler-not "lxvd2x"   } } */
/* { dg-final { scan-assembler-not "lxvw4x"   } } */
/* { dg-final { scan-assembler-not "lxvx"     } } */
/* { dg-final { scan-assembler-not "lxv"      } } */

/* With recent enhancements to the code generator, it is considered
 * legal to implement vec_extract with lvx and xxpermdi.  Previous
 * versions of this test forbid both instructions.  */
