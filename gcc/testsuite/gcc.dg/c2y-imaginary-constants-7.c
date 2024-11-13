/* Test that imaginary constants are accepted in C2Y mode: -pedantic.  */
/* { dg-do run } */
/* { dg-options "-std=c2y -pedantic" } */
/* { dg-add-options float16 } */
/* { dg-require-effective-target float16 } */

#include "c23-imaginary-constants-7.c"
