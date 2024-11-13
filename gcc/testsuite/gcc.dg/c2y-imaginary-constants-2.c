/* Test that imaginary constants are accepted in C2Y mode: compat warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -Wc23-c2y-compat" } */

_Complex float a = 1.if;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex float b = 2.Fj;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex float c = 3.fI;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex float d = 4.JF;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex double e = 1.i;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex double f = 2.j;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex double g = 3.I;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex double h = 4.J;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex long double i = 1.il;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex long double j = 2.Lj;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex long double k = 3.lI;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex long double l = 4.JL;	/* { dg-warning "imaginary constants are a C2Y feature" } */
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
