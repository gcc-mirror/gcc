/* Test that integral imaginary constants are diagnosed in C2Y mode: -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu2y -pedantic-errors" } */

_Complex float a = 1i;		/* { dg-error "imaginary constants are a GCC extension" } */
_Complex float b = 2j;		/* { dg-error "imaginary constants are a GCC extension" } */
_Complex float c = 3I;		/* { dg-error "imaginary constants are a GCC extension" } */
_Complex float d = 4J;		/* { dg-error "imaginary constants are a GCC extension" } */
_Complex double e = 1il;	/* { dg-error "imaginary constants are a GCC extension" } */
_Complex double f = 2Lj;	/* { dg-error "imaginary constants are a GCC extension" } */
_Complex double g = 3lI;	/* { dg-error "imaginary constants are a GCC extension" } */
_Complex double h = 4JL;	/* { dg-error "imaginary constants are a GCC extension" } */
_Complex long double i = 1ill;	/* { dg-error "imaginary constants are a GCC extension" } */
_Complex long double j = 2LLj;	/* { dg-error "imaginary constants are a GCC extension" } */
_Complex long double k = 3llI;	/* { dg-error "imaginary constants are a GCC extension" } */
_Complex long double l = 4JLL;	/* { dg-error "imaginary constants are a GCC extension" } */
__extension__ _Complex float m = 1i;
__extension__ _Complex float n = 2j;
__extension__ _Complex float o = 3I;
__extension__ _Complex float p = 4J;
__extension__ _Complex double q = 1il;
__extension__ _Complex double r = 2Lj;
__extension__ _Complex double s = 3lI;
__extension__ _Complex double t = 4JL;
__extension__ _Complex long double u = 1ill;
__extension__ _Complex long double v = 2LLj;
__extension__ _Complex long double w = 3llI;
__extension__ _Complex long double x = 4JLL;
