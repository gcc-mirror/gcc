/* { dg-do compile } */
/* { dg-require-effective-target stdint_types } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fno-tree-loop-vectorize" } */
/* { dg-add-options arm_v8_3a_complex_neon } */

#define UNROLL

#define TYPE uint16_t
#define N 16
#include <stdint.h>
#include "complex-add-pattern-template.c"

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT90" 1 "slp1" { target { vect_complex_add_short } } } } */
/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT270" 1 "slp1" { target { vect_complex_add_short } && ! target { aarch64_sve2 } } } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT270" "slp1" } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT90" "slp1" } } */
