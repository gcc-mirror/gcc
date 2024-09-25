/* PR target/54700 */
/* { dg-do run { target avx } } */
/* { dg-options "-O2 -std=c++14 -mavx -mno-xop -mno-avx2" } */
/* { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } } */

#define CHECK_H "avx-check.h"
#define TEST avx_test

#include "sse4_1-pr54700-2.C"
