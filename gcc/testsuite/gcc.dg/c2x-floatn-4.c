/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-add-options float64x } */
/* { dg-require-effective-target float64x } */

_Float64x a
  = 1.0F64x;
__extension__ _Float64x b
  = 2.0F64x;
