/* PR target/77476 */
/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#include "avx512dq-check.h"
#define PR77476_TEST avx512dq_test

#include "avx512f-pr77476.c"
