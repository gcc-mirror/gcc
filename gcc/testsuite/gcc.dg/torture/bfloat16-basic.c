/* Test __bf16.  */
/* { dg-do run } */
/* { dg-options "-Wno-old-style-definition" } */
/* { dg-add-options bfloat16 } */
/* { dg-require-effective-target bfloat16_runtime } */

#define TYPE __bf16
#define CST(C) CONCAT (C, bf16)
#define CSTU(C) CONCAT (C, BF16)

#include "floatn-basic.h"
