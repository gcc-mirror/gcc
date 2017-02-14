/* PR target/77476 */
/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#include "avx512bw-check.h"
#define PR77476_TEST avx512bw_test

#include "avx512f-pr77476.c"
