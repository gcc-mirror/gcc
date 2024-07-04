/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

/* GCC 7.1 did not have a specialized method for inserting 32-bit floating
   point on ISA 3.0 (power9) systems.  */

vector float
insert_arg_0 (vector float vf, float f)
{
  return vec_insert (f, vf, 0);
}

vector float
insert_arg_1 (vector float vf, float f)
{
  return vec_insert (f, vf, 1);
}

vector float
insert_arg_2 (vector float vf, float f)
{
  return vec_insert (f, vf, 2);
}

vector float
insert_arg_3 (vector float vf, float f)
{
  return vec_insert (f, vf, 3);
}

/* { dg-final { scan-assembler     {\mxscvdpspn\M} } } */
/* { dg-final { scan-assembler     {\mxxinsertw\M} } } */
/* { dg-final { scan-assembler-not {\mlvewx\M}     } } */
/* { dg-final { scan-assembler-not {\mlvx\M}       } } */
/* { dg-final { scan-assembler-not {\mvperm\M}     } } */
/* { dg-final { scan-assembler-not {\mvpermr\M}    } } */
/* { dg-final { scan-assembler-not {\mstfs\M}      } } */
/* { dg-final { scan-assembler-not {\mstxssp\M}    } } */
/* { dg-final { scan-assembler-not {\mstxsspx\M}   } } */
