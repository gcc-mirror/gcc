/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

double a = 1.ld;	/* { dg-error "12:invalid suffix" } */
double b = 1.fd;	/* { dg-error "12:invalid suffix" } */
double c = 1.di;	/* { dg-error "12:invalid suffix" } */
double d = 1.dj;	/* { dg-error "12:invalid suffix" } */
double e = 1.id;	/* { dg-error "12:invalid suffix" } */
double f = 1.jd;	/* { dg-error "12:invalid suffix" } */
double g = 1.ddd;	/* { dg-error "12:invalid suffix" } */
double h = 1.ldd;	/* { dg-error "12:invalid suffix" } */
double i = 1.dld;	/* { dg-error "12:invalid suffix" } */
double j = 1.ddl;	/* { dg-error "12:invalid suffix" } */
double k = 1.fdd;	/* { dg-error "12:invalid suffix" } */
double l = 1.dfd;	/* { dg-error "12:invalid suffix" } */
double m = 1.ddf;	/* { dg-error "12:invalid suffix" } */
