/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-add-options float32 } */
/* { dg-add-options float64 } */
/* { dg-add-options float32x } */
/* { dg-require-effective-target float32 } */
/* { dg-require-effective-target float32x } */
/* { dg-require-effective-target float64 } */

_Float32 a
  = 1.0F32;
_Float64 b
  = 1.0F64;
_Float32x c
  = 1.0F32x;
__extension__ _Float32 d
  = 2.0F32;
__extension__ _Float64 e
  = 2.0F64;
__extension__ _Float32x f
  = 2.0F32x;
