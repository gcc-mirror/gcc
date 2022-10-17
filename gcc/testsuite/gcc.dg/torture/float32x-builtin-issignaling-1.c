/* Test _Float32x __builtin_issignaling.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float32x } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float32x_runtime } */
/* { dg-additional-options "-fsignaling-nans" } */
/* Workaround for PR57484 on ia32: */
/* { dg-additional-options "-msse2 -mfpmath=sse" { target { ia32 && sse2_runtime } } } */

#define WIDTH 32
#define EXT 1
#include "builtin-issignaling-1.c"
