/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

#include <altivec.h>

/* Insure setting 0.0f to a V4SFmode element does not do a FP conversion.  */

vector float
insert_arg_0 (vector float vf)
{
  return vec_insert (0.0f, vf, 0);
}

/* { dg-final { scan-assembler     {\mxxinsertw\M}   } } */
/* { dg-final { scan-assembler-not {\mlvewx\M}       } } */
/* { dg-final { scan-assembler-not {\mlvx\M}         } } */
/* { dg-final { scan-assembler-not {\mvperm\M}       } } */
/* { dg-final { scan-assembler-not {\mvpermr\M}      } } */
/* { dg-final { scan-assembler-not {\mstfs\M}        } } */
/* { dg-final { scan-assembler-not {\mstxssp\M}      } } */
/* { dg-final { scan-assembler-not {\mstxsspx\M}     } } */
/* { dg-final { scan-assembler-not {\mxscvdpspn\M}   } } */
/* { dg-final { scan-assembler-not {\mxxextractuw\M} } } */
