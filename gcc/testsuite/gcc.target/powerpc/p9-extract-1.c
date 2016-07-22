/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9 -O2" } */

#include <altivec.h>

int extract_int_0 (vector int a) { return vec_extract (a, 0); }
int extract_int_3 (vector int a) { return vec_extract (a, 3); }

int extract_short_0 (vector short a) { return vec_extract (a, 0); }
int extract_short_3 (vector short a) { return vec_extract (a, 7); }

int extract_schar_0 (vector signed char a) { return vec_extract (a, 0); }
int extract_schar_3 (vector signed char a) { return vec_extract (a, 15); }

/* { dg-final { scan-assembler     "vextractub"  } } */
/* { dg-final { scan-assembler     "vextractuh"  } } */
/* { dg-final { scan-assembler     "xxextractuw" } } */
/* { dg-final { scan-assembler     "mfvsrd"      } } */
/* { dg-final { scan-assembler-not "stxvd2x"     } } */
/* { dg-final { scan-assembler-not "stxv"        } } */
/* { dg-final { scan-assembler-not "lwa"         } } */
/* { dg-final { scan-assembler-not "lwz"         } } */
/* { dg-final { scan-assembler-not "lha"         } } */
/* { dg-final { scan-assembler-not "lhz"         } } */
