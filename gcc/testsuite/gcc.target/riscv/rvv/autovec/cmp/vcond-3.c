/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -fno-trapping-math -fno-vect-cost-model" } */

/* The difference here is that nueq can use LTGT.  */

#include "vcond-2.c"

/* { dg-final { scan-assembler-times {\tvmfeq} 90 } } */
/* { dg-final { scan-assembler-times {\tvmfne} 6 } } */
/* { dg-final { scan-assembler-times {\tvmfgt} 30 } } */
/* { dg-final { scan-assembler-times {\tvmflt} 30 } } */
/* { dg-final { scan-assembler-times {\tvmfge} 18 } } */
/* { dg-final { scan-assembler-times {\tvmfle} 18 } } */
