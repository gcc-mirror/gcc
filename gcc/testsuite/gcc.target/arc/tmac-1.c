/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=archs -mmpy-option=8" } */

/* Test MAC operation for MPY_OPTION = 8.  */
#include "tmac.h"

/* { dg-final { scan-assembler "macd " } } */
/* { dg-final { scan-assembler "macdu" } } */
/* { dg-final { scan-assembler "mpyd " } } */
/* { dg-final { scan-assembler "mpydu" } } */

