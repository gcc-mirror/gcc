/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#define TYPE uint16_t
#define ITYPE int16_t
#include "struct_vect_7_run.c"
