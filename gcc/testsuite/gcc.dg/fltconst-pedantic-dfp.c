/* { dg-do compile } */
/* { dg-options "-std=gnu17 -pedantic" } */

double a = 1.dl;	/* { dg-warning "decimal float" } */
/* { dg-error "not supported for this target" "not supported" { target { ! dfp } } .-1 } */
double b = 1.df;	/* { dg-warning "decimal float" } */
/* { dg-error "not supported for this target" "not supported" { target { ! dfp } } .-1 } */
double c = 1.dd;	/* { dg-warning "decimal float" } */
/* { dg-error "not supported for this target" "not supported" { target { ! dfp } } .-1 } */
