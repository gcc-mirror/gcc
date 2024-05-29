/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-skip-if "" { powerpc*le-*-* } } */
/* { dg-options "-mdejagnu-cpu=power7 -mdejagnu-tune=power8 -O3" } */
/* { dg-require-effective-target powerpc_vsx } */

vector double fusion_vector (vector double *p) { return p[2]; }

/* { dg-final { scan-assembler-times "vector load fusion" 1 } } */
