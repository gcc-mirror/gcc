/* { dg-do run { target { { ! x32 } && avx512f_runtime } } } */
/* { dg-do compile { target { { ! x32 } && { ! avx512f_runtime } } } } */
/* { dg-options "-Ofast -mabi=ms -mavx512f" } */
/* { dg-require-effective-target avx512f } */

/* Test with avx512, va_args, and ms to sysv call.  */

#define CALLEE_ABI sysv_abi
#include "pr80969-4.h"
