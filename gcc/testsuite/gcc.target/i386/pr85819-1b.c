/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mfpmath=sse" } */

#include "pr85819-1a.c"

/* { dg-final { scan-assembler "vcvtusi2ss" } } */
