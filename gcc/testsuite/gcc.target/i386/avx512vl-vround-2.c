/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target avx512vl } */
/* { dg-options "-Ofast -mavx512vl" } */

#define CHECK_H "avx512vl-check.h"
#define TEST avx512vl_test
#define SRC "avx512vl-vround-1.c"

#include "avx-vround-2.c"
