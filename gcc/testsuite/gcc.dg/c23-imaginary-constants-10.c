/* Test that imaginary constants are diagnosed in C23 mode: -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-add-options float64x } */
/* { dg-require-effective-target float64x } */

_Complex _Float64x a = 1.if64x;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float64x b = 2.F64xj;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float64x c = 3.f64xi;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float64x d = 4.JF64x;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
__extension__ _Complex _Float64x e = 1.if64x;
__extension__ _Complex _Float64x f = 2.F64xj;
__extension__ _Complex _Float64x g = 3.f64xi;
__extension__ _Complex _Float64x h = 4.JF64x;
