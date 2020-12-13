/* { dg-do compile } */
/* { dg-require-effective-target vect_complex_add_byte } */
/* { dg-require-effective-target stdint_types } */
/* { dg-add-options arm_v8_1m_mve_fp } */

#define TYPE uint8_t
#define N 200
#include <stdint.h>
#include "complex-add-pattern-template.c"

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT90" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT270" 1 "vect" } } */
