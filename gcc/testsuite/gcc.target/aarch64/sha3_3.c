/* { dg-do compile } */
/* { dg-options "-march=armv8.4-a+sha3" } */

#include "sha3.h"

/* { dg-final { scan-assembler-times "eor3\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b, v\[0-9\]+\.16b, v\[0-9\]+\.16b" 8 } } */
/* { dg-final { scan-assembler-times "rax1\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d, v\[0-9\]+\.2d" 1 } } */
/* { dg-final { scan-assembler-times "xar\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d, v\[0-9\]+\.2d, 15" 1 } } */
/* { dg-final { scan-assembler-times "bcax\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b, v\[0-9\]+\.16b, v\[0-9\]+\.16b" 8 } } */
