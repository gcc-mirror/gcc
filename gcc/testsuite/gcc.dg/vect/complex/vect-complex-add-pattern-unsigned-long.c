/* { dg-do compile } */
/* { dg-require-effective-target stdint_types } */
/* { dg-add-options arm_v8_3a_complex_neon } */

#define UNROLL

#define TYPE uint64_t
#define N 200
#include <stdint.h>
#include "complex-add-pattern-template.c"

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT90" 1 "vect" { target { vect_complex_add_long } } } } */
/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT270" 1 "vect" { target { vect_complex_add_long } } } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT270" "vect" { target { vect_long_long } } } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT90" "vect" { target { vect_long_long } } } } */
