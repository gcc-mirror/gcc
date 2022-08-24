/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target arm_v8_2a_dotprod_neon_hw { target { aarch64*-*-* || arm*-*-* } } } */
/* { dg-add-options arm_v8_2a_dotprod_neon }  */

#define SIGNEDNESS_1 unsigned
#define SIGNEDNESS_2 unsigned
#define SIGNEDNESS_3 unsigned
#define SIGNEDNESS_4 signed

#include "vect-reduc-dot-9.c"

/* { dg-final { scan-tree-dump-not "vect_recog_dot_prod_pattern: detected" "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" { target vect_sdot_qi } } } */
