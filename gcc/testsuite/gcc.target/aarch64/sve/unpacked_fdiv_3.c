/* { dg-do compile } */
/* { dg-options "-Ofast -moverride=sve_width=2048 -mlow-precision-div" } */

#include "unpacked_fdiv_1.c"

/* { dg-final { scan-assembler-not {\tfrecpe\tz[0-9]+\.h} } } */
/* { dg-final { scan-assembler-not {\tfrecps\tz[0-9]+\.h} } } */

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 3 } } */

/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.s} 2 } } */
/* { dg-final { scan-assembler-times {\tfrecpe\tz[0-9]+\.s} 1 } } */
/* { dg-final { scan-assembler-times {\tfrecps\tz[0-9]+\.s} 1 } } */
