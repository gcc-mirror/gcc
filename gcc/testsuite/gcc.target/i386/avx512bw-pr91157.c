/* PR tree-optimization/91157 */
/* { dg-do run { target { avx512bw && lp64 } } } */
/* { dg-options "-O2 -mavx512bw -fexceptions -fnon-call-exceptions -fsignaling-nans" } */

#define AVX512BW
#include "avx512f-pr91157.c"
