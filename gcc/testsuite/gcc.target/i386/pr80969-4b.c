/* { dg-do run { target { lp64 && avx512f_runtime } } } */
/* { dg-do compile { target { lp64 && { ! avx512f_runtime } } } } */
/* { dg-options "-Ofast -mabi=ms -mavx512f -mcall-ms2sysv-xlogues" } */
/* { dg-require-effective-target avx512f } */

/* Test with avx512, va_args, and ms to sysv call using save/restore stubs.  */

#define CALLEE_ABI sysv_abi
#include "pr80969-4.h"
