/* { dg-do run } */
/* { dg-require-effective-target fma } */
/* { dg-options "-O3 -Wno-attributes -mfpmath=sse -mfma" } */

/* Test that the compiler properly optimizes floating point multiply
   and add instructions into FMA3 instructions.  */

#define TYPE float

#include "l_fma_6.h"

#include "fma_run_float_results_6.h"

#include "fma-check.h"
#include "l_fma_main.h"
