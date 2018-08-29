/* { dg-do compile } */
/* { dg-options "-march=armv8.2-a+sha3" } */

#include "sha2.h"

/* { dg-final { scan-assembler-times "sha512h\\tq\[0-9\]+, q\[0-9\]+, v\[0-9\]+\.2d" 1 } } */
/* { dg-final { scan-assembler-times "sha512h2\\tq\[0-9\]+, q\[0-9\]+, v\[0-9\]+\.2d" 1 } } */
/* { dg-final { scan-assembler-times "sha512su0\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" 1 } } */
/* { dg-final { scan-assembler-times "sha512su1\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d, v\[0-9\]+\.2d" 1 } } */
