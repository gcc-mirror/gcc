/* { dg-do compile } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target has_arch_ppc64 } */

#include <altivec.h>

#ifdef __BIG_ENDIAN__
#define DWORD0_FIRST_SHORT 3
#define DWORD0_FIRST_CHAR 7
#else
#define DWORD0_FIRST_SHORT 4
#define DWORD0_FIRST_CHAR 8
#endif

void vec_extract_short (vector short v, short* p)
{
   *p = vec_extract(v, DWORD0_FIRST_SHORT);
}

void vec_extract_char (vector char v, char* p)
{
   *p = vec_extract(v, DWORD0_FIRST_CHAR);
}

/* { dg-final { scan-assembler-times {\mstxsi[hb]x\M} 2 } } */
/* { dg-final { scan-assembler-not {\mvextractu[hb]\M} } } */
