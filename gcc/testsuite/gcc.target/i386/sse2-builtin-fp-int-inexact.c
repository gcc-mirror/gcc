/* Test -fno-fp-int-builtin-inexact for SSE 2.  */
/* { dg-do run } */
/* { dg-options "-O2 -msse2 -mfpmath=sse -fno-fp-int-builtin-inexact" } */
/* { dg-add-options c99_runtime } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"

#define main_test sse2_test
#define ARCH_MAIN
#include "../../gcc.dg/torture/builtin-fp-int-inexact.c"
