/* { dg-do run { target { lp64 && avx512f_runtime } } } */
/* { dg-do compile { target { lp64 && { ! avx512f_runtime } } } } */
/* { dg-options "-Ofast -mabi=ms -mavx512f -mcall-ms2sysv-xlogues" } */
/* { dg-require-effective-target avx512f } */

/* Test when calling a sysv func using save/restore stubs.  */

#include "pr80969-2.c"
