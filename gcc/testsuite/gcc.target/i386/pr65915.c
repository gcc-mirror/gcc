/* { dg-do run } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -mavx512f -fpic -mcmodel=medium" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target lp64 } */

#include "avx512f-vrndscalepd-2.c"
