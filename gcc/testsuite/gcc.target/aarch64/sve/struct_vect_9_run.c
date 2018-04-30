/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#define TYPE uint32_t
#define ITYPE int32_t
#include "struct_vect_7_run.c"
