/* Test -fno-fp-int-builtin-inexact for SSE 4.1.  */
/* { dg-do run } */
/* { dg-options "-O2 -msse4.1 -mfpmath=sse -fno-fp-int-builtin-inexact" } */
/* { dg-add-options c99_runtime } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-require-effective-target sse4 } */

#include "sse4_1-check.h"

#define main_test sse4_1_test
#define ARCH_MAIN
#include "../../gcc.dg/torture/builtin-fp-int-inexact.c"
