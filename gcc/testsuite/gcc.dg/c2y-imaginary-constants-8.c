/* Test that imaginary constants are accepted in C2Y mode: compat warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -Wc23-c2y-compat" } */
/* { dg-add-options float16 } */
/* { dg-require-effective-target float16 } */

_Complex _Float16 a = 1.if16;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float16 b = 2.F16j;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float16 c = 3.f16i;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float16 d = 4.JF16;	/* { dg-warning "imaginary constants are a C2Y feature" } */
__extension__ _Complex _Float16 e = 1.if16;
__extension__ _Complex _Float16 f = 2.F16j;
__extension__ _Complex _Float16 g = 3.f16i;
__extension__ _Complex _Float16 h = 4.JF16;
