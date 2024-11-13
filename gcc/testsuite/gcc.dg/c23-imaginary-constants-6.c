/* Test that imaginary constants are diagnosed in C23 mode: -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-add-options float128 } */
/* { dg-require-effective-target float128 } */

_Complex _Float128 a = 1.if128;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float128 b = 2.F128j;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float128 c = 3.f128i;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float128 d = 4.JF128;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
__extension__ _Complex _Float128 e = 1.if128;
__extension__ _Complex _Float128 f = 2.F128j;
__extension__ _Complex _Float128 g = 3.f128i;
__extension__ _Complex _Float128 h = 4.JF128;
