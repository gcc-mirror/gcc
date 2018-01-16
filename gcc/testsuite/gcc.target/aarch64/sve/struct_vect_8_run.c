/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#define TYPE unsigned short
#define ITYPE short
#include "struct_vect_7_run.c"
