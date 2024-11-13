/* Test that imaginary constants are accepted in C2Y mode: -pedantic.  */
/* { dg-do run } */
/* { dg-options "-std=c2y -pedantic" } */
/* { dg-add-options float32 } */
/* { dg-add-options float64 } */
/* { dg-add-options float32x } */
/* { dg-require-effective-target float32 } */
/* { dg-require-effective-target float32x } */
/* { dg-require-effective-target float64 } */

#include "c23-imaginary-constants-3.c"
