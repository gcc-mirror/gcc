/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */
/* { dg-require-effective-target vect_float } */
/* { dg-add-options arm_v8_3a_complex_neon } */

#define TYPE float
#define N 200
#include "complex-add-template.c"

/* { dg-final { scan-tree-dump-times "add new stmt: \[^\n\r]*COMPLEX_ADD_ROT270" 2 "vect" { target { vect_complex_add_float } } } } */
/* { dg-final { scan-tree-dump-times "add new stmt: \[^\n\r]*COMPLEX_ADD_ROT90" 2 "vect" { target { vect_complex_add_float } } } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT270" "vect" } } */
/* { dg-final { scan-tree-dump "Found COMPLEX_ADD_ROT90" "vect" } } */
