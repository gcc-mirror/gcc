/* { dg-do run } */
/* { dg-require-effective-target arm_v8_neon_hw } */
/* { dg-options "-O2 -ftree-vectorize -fno-inline -funsafe-math-optimizations" } */
/* { dg-add-options arm_v8_neon } */

#include "fmaxmin.x"
