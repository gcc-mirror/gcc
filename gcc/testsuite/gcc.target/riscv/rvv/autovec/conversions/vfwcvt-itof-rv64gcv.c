/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=scalable" } */

#include "vfwcvt-itof-template.h"

/* Conversions that the vectorizer does via multiple intermediate
   types end up as signed conversions.  */
/* { dg-final { scan-assembler-times {\tvfwcvt\.f\.x\.v} 9 } } */
/* { dg-final { scan-assembler-times {\tvfwcvt\.f\.xu\.v} 3 } } */
