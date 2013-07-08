/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

/* Compile-only test to ensure that expressions can be passed to
   Altivec builtins without error.  */

#include <altivec.h>

void func(unsigned char *buf, unsigned len)
{
        vec_dst(buf, (len >= 256 ? 0 : len) | 512, 2);
}
