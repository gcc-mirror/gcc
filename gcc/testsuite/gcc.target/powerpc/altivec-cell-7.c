/* { dg-do compile  } */
/* { dg-options "-O2 -maltivec -mabi=altivec -mdejagnu-cpu=cell" } */
/* { dg-require-effective-target powerpc_altivec } */
/* { dg-final { scan-assembler-times "vor" 2 } } */
#include <altivec.h>

/* Make sure that lvlx and lvrx are not combined into one insn and
   we still get a vor. */

vector unsigned char
lvx_float (long off, float *p)
{
    vector unsigned char l, r;

    l = (vector unsigned char) vec_lvlx (off, p);
    r = (vector unsigned char) vec_lvrx (off, p);
    return vec_or(l, r);
}

vector unsigned char
lvxl_float (long off, float *p)
{
    vector unsigned char l, r;

    l = (vector unsigned char) vec_lvlxl (off, p);
    r = (vector unsigned char) vec_lvrxl (off, p);
    return vec_or(l, r);
}
