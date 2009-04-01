/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic" } */

double a = 1.d;		/* { dg-warning "double constant" } */
double b = 1.D;		/* { dg-warning "double constant" } */
double c = 1.1e+2d;	/* { dg-warning "double constant" } */

double d = 1.di;	/* { dg-warning "imaginary constants" } */
double e = 1.dj;	/* { dg-warning "imaginary constants" } */
double f = 1.id;	/* { dg-warning "imaginary constants" } */
double g = 1.jd;	/* { dg-warning "imaginary constants" } */

double h = 0x1.5p1d;	/* { dg-warning "double constant" } */
