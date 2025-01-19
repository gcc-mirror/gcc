/* Test that imaginary constants are diagnosed in C23 mode: -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-add-options float16 } */
/* { dg-require-effective-target float16 } */

_Complex _Float16 a = 1.if16;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float16 b = 2.F16j;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float16 c = 3.f16i;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex _Float16 d = 4.JF16;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
__extension__ _Complex _Float16 e = 1.if16;
__extension__ _Complex _Float16 f = 2.F16j;
__extension__ _Complex _Float16 g = 3.f16i;
__extension__ _Complex _Float16 h = 4.JF16;
