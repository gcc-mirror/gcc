/* { dg-do run } */
/* { dg-require-effective-target vaes } */
/* { dg-options "-O2 -maes -mavx" } */

#define CHECK_H "aes-avx-check.h"
#define TEST aes_avx_test

#include "aesenclast.c"
