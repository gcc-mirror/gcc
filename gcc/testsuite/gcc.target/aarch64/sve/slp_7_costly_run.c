/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -fno-vect-cost-model" } */

#define FILENAME "slp_7_costly.c"
#include "slp_7_run.c"
