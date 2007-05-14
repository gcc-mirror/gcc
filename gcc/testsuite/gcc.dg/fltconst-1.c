/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

double a = 1.ld;	/* { dg-error "invalid suffix" } */
double b = 1.fd;	/* { dg-error "invalid suffix" } */
double c = 1.di;	/* { dg-error "invalid suffix" } */
double d = 1.dj;	/* { dg-error "invalid suffix" } */
double e = 1.id;	/* { dg-error "invalid suffix" } */
double f = 1.jd;	/* { dg-error "invalid suffix" } */
double g = 1.ddd;	/* { dg-error "invalid suffix" } */
double h = 1.ldd;	/* { dg-error "invalid suffix" } */
double i = 1.dld;	/* { dg-error "invalid suffix" } */
double j = 1.ddl;	/* { dg-error "invalid suffix" } */
double k = 1.fdd;	/* { dg-error "invalid suffix" } */
double l = 1.dfd;	/* { dg-error "invalid suffix" } */
double m = 1.ddf;	/* { dg-error "invalid suffix" } */
