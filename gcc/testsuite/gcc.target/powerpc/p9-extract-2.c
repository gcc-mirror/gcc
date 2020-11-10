/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

#include <altivec.h>

void extract_int_0 (int *p, vector int a) { *p = vec_extract (a, 0); }
void extract_int_3 (int *p, vector int a) { *p = vec_extract (a, 3); }

void extract_short_0 (short *p, vector short a) { *p = vec_extract (a, 0); }
void extract_short_3 (short *p, vector short a) { *p = vec_extract (a, 7); }

void extract_schar_0 (signed char *p, vector signed char a) { *p = vec_extract (a, 0); }
void extract_schar_3 (signed char *p, vector signed char a) { *p = vec_extract (a, 15); }

/* { dg-final { scan-assembler     "vextractub"      } } */
/* { dg-final { scan-assembler     "vextractuh"      } } */
/* { dg-final { scan-assembler     "xxextractuw"     } } */
/* { dg-final { scan-assembler     "stxsibx"         } } */
/* { dg-final { scan-assembler     "stxsihx"         } } */
/* { dg-final { scan-assembler     "stfiwx\|stxsiwx" } } */
/* { dg-final { scan-assembler-not "mfvsrd"          } } */
/* { dg-final { scan-assembler-not "stxvd2x"         } } */
/* { dg-final { scan-assembler-not "stxv"            } } */
/* { dg-final { scan-assembler-not "lwa"             } } */
/* { dg-final { scan-assembler-not "stw"             } } */
