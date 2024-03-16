/* { dg-do compile } */
/* { dg-options "-std=c11 -Wc11-c23-compat" } */
/* { dg-add-options float32 } */
/* { dg-add-options float64 } */
/* { dg-add-options float32x } */
/* { dg-require-effective-target float32 } */
/* { dg-require-effective-target float32x } */
/* { dg-require-effective-target float64 } */

_Float32 a		/* { dg-warning "ISO C does not support the '_Float32' type before C23" } */
  = 1.0F32;		/* { dg-warning "non-standard suffix on floating constant before C23" } */
_Float64 b		/* { dg-warning "ISO C does not support the '_Float64' type before C23" } */
  = 1.0F64;		/* { dg-warning "non-standard suffix on floating constant before C23" } */
_Float32x c		/* { dg-warning "ISO C does not support the '_Float32x' type before C23" } */
  = 1.0F32x;		/* { dg-warning "non-standard suffix on floating constant before C23" } */
__extension__ _Float32 d
  = 2.0F32;
__extension__ _Float64 e
  = 2.0F64;
__extension__ _Float32x f
  = 2.0F32x;
