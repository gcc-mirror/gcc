/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -fno-vect-cost-model" } */

#pragma GCC target "+nosve"

#include "vect-fmaxv-fminv.x"

/* { dg-final { scan-assembler "fminnmv" } } */
/* { dg-final { scan-assembler "fmaxnmv" } } */
/* { dg-final { scan-assembler "fminnmp" } } */
/* { dg-final { scan-assembler "fmaxnmp" } } */
