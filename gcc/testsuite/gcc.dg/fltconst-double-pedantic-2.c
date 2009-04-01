/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

double a = 1.d;		/* { dg-error "double constant" } */
double b = 1.D;		/* { dg-error "double constant" } */
double c = 1.1e+2d;	/* { dg-error "double constant" } */

double d = 1.di;	/* { dg-error "imaginary constants" } */
double e = 1.dj;	/* { dg-error "imaginary constants" } */
double f = 1.id;	/* { dg-error "imaginary constants" } */
double g = 1.jd;	/* { dg-error "imaginary constants" } */

double h = 0x1.5p1d;	/* { dg-error "double constant" } */
