/* Test that imaginary constants are accepted in C2Y mode: compat warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -Wc23-c2y-compat" } */
/* { dg-add-options float64x } */
/* { dg-require-effective-target float64x } */

_Complex _Float64x a = 1.if64x;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float64x b = 2.F64xj;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float64x c = 3.f64xi;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float64x d = 4.JF64x;	/* { dg-warning "imaginary constants are a C2Y feature" } */
__extension__ _Complex _Float64x e = 1.if64x;
__extension__ _Complex _Float64x f = 2.F64xj;
__extension__ _Complex _Float64x g = 3.f64xi;
__extension__ _Complex _Float64x h = 4.JF64x;
