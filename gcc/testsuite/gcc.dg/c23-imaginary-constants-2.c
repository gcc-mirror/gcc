/* Test that imaginary constants are diagnosed in C23 mode: -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

_Complex float a = 1.if;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex float b = 2.Fj;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex float c = 3.fI;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex float d = 4.JF;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex double e = 1.i;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex double f = 2.j;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex double g = 3.I;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex double h = 4.J;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex long double i = 1.il;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex long double j = 2.Lj;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex long double k = 3.lI;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
_Complex long double l = 4.JL;	/* { dg-error "imaginary constants are a C2Y feature or GCC extension" } */
__extension__ _Complex float m = 1.if;
__extension__ _Complex float n = 2.Fj;
__extension__ _Complex float o = 3.fI;
__extension__ _Complex float p = 4.JF;
__extension__ _Complex double q = 1.i;
__extension__ _Complex double r = 2.j;
__extension__ _Complex double s = 3.I;
__extension__ _Complex double t = 4.J;
__extension__ _Complex long double u = 1.il;
__extension__ _Complex long double v = 2.Lj;
__extension__ _Complex long double w = 3.lI;
__extension__ _Complex long double x = 4.JL;
