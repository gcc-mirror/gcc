/* Test that imaginary constants are accepted in C2Y mode: compat warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -Wc23-c2y-compat" } */
/* { dg-add-options float128 } */
/* { dg-require-effective-target float128 } */

_Complex _Float128 a = 1.if128;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float128 b = 2.F128j;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float128 c = 3.f128i;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float128 d = 4.JF128;	/* { dg-warning "imaginary constants are a C2Y feature" } */
__extension__ _Complex _Float128 e = 1.if128;
__extension__ _Complex _Float128 f = 2.F128j;
__extension__ _Complex _Float128 g = 3.f128i;
__extension__ _Complex _Float128 h = 4.JF128;
