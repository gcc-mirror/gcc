/* Test _Float32 __builtin_issignaling.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float32 } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float32_runtime } */
/* { dg-additional-options "-fsignaling-nans" } */
/* Workaround for PR57484 on ia32: */
/* { dg-additional-options "-msse2 -mfpmath=sse" { target { ia32 && sse2_runtime } } } */

#define WIDTH 32
#define EXT 0
#include "builtin-issignaling-1.c"
