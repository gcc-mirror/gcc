/* { dg-do compile } */
/* { dg-require-effective-target stdint_types } */
/* { dg-require-effective-target vect_int } */
/* { dg-add-options arm_v8_1m_mve_fp } */

#define TYPE int8_t
#define N 200
#include <stdint.h>
#include "complex-add-pattern-template.c"

/* { dg-final { scan-tree-dump-times "add new stmt: \[^\n\r]*COMPLEX_ADD_ROT90" 3 "vect" { target { vect_complex_add_byte } xfail { *-*-* } } } } */
/* { dg-final { scan-tree-dump-times "add new stmt: \[^\n\r]*COMPLEX_ADD_ROT270" 1 "vect" { target { vect_complex_add_byte } xfail { *-*-* } } } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT270" "vect" } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT90" "vect" } } */
