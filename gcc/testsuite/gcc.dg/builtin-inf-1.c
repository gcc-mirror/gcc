/* { dg-do compile } */

float fi = __builtin_inff();
double di = __builtin_inf();
long double li = __builtin_infl();

float fh = __builtin_huge_valf();
double dh = __builtin_huge_val();
long double lh = __builtin_huge_vall();

/* { dg-warning "does not support infinity" "INF unsupported" { target vax-*-* c4x-*-* } 3 } */
/* { dg-warning "does not support infinity" "INF unsupported" { target vax-*-* c4x-*-* } 4 } */
/* { dg-warning "does not support infinity" "INF unsupported" { target vax-*-* c4x-*-* } 5 } */
