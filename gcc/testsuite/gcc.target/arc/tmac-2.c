/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-O2 -mcpu=archs -mmpy-option=7" } */

/* Test MAC operation for MPY_OPTION = 7.  */
#include "tmac.h"

/* { dg-final { scan-assembler "mac " } } */
/* { dg-final { scan-assembler "macu" } } */
/* { dg-final { scan-assembler "mpym\\t" } } */
/* { dg-final { scan-assembler "mpymu" } } */
