/* PR target/100648  */
/* { dg-do run { target avx2 } } */
/* { dg-options "-O2 -std=c++14 -mavx2 -mavx512vl -mavx512bw" } */

#ifndef CHECK_H
#define CHECK_H "avx512f-helper.h"
#endif

#ifndef TEST
#define TEST_test_256
#endif

#include CHECK_H
#include "avx2-pr54700-2.C"

#define AVX512VL
#define AVX512BW
