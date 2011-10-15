/* { dg-do run } */
/* { dg-require-effective-target fma } */
/* { dg-options "-O3 -Wno-attributes -mfpmath=sse -mfma" } */

/* Test that the compiler properly optimizes floating point multiply
   and add instructions into FMA3 instructions.  */

#define TYPE double

#include "fma_6.h"

#include "fma_run_double_results_6.h"

#include "fma-check.h"
#include "fma_main.h"
