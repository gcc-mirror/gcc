/* Test that imaginary constants are accepted in C2Y mode: -pedantic.  */
/* { dg-do run } */
/* { dg-options "-std=c2y -pedantic" } */
/* { dg-add-options float64x } */
/* { dg-require-effective-target float64x } */

#include "c23-imaginary-constants-9.c"
