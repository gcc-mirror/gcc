/* { dg-do compile } */
/* { dg-options "-std=gnu99 -fshow-column" } */

double a = 1.ld;	/* { dg-error "12:invalid suffix" } */
double b = 1.fd;	/* { dg-error "12:invalid suffix" } */
double c = 1.di;
double d = 1.dj;
double e = 1.id;
double f = 1.jd;
double g = 1.ddd;	/* { dg-error "12:invalid suffix" } */
double h = 1.ldd;	/* { dg-error "12:invalid suffix" } */
double i = 1.dld;	/* { dg-error "12:invalid suffix" } */
double j = 1.ddl;	/* { dg-error "12:invalid suffix" } */
double k = 1.fdd;	/* { dg-error "12:invalid suffix" } */
double l = 1.dfd;	/* { dg-error "12:invalid suffix" } */
double m = 1.ddf;	/* { dg-error "12:invalid suffix" } */
