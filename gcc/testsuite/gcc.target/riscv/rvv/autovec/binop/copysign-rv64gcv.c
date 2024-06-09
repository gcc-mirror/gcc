/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -O3 -fno-vect-cost-model -march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=zvl -ffast-math" } */

#include "copysign-template.h"

/* { dg-final { scan-assembler-times {\tvfsgnj\.vv} 6 } } */
/* The vectorizer wraps scalar variants of copysign into vector constants which
   expand cannot handle currently.  Therefore only expect 3 instead of 6
   vfsgnjx.vv.  */
/* { dg-final { scan-assembler-times {\tvfsgnjx\.vv} 3 } } */
/* { dg-final { scan-assembler-times {\tvfsgnjn\.vv} 6 } } */
