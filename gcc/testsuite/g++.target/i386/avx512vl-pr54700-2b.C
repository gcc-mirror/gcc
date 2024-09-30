/* PR target/pr100648 */
/* { dg-do run { target sse4 } } */
/* { dg-options "-O2 -std=c++14 -msse4 -mavx512vl -mavx512bw -mno-xop" } */
/* { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } } */

#ifndef CHECK_H
#define CHECK_H "avx512f-helper.h"
#endif

#ifndef TEST
#define TEST_test_128
#endif

#include CHECK_H
#include "sse4_1-pr54700-2.C"

#define AVX512VL
#define AVX512BW
