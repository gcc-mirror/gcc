/* { dg-do compile } */

float fi = __builtin_inff();
/* { dg-error "does not support infinity" "INF unsupported" { target pdp11*-*-* vax-*-* spu-*-* } .-1 } */
double di = __builtin_inf();
/* { dg-error "does not support infinity" "INF unsupported" { target pdp11*-*-* vax-*-* } .-1 } */
long double li = __builtin_infl();
/* { dg-error "does not support infinity" "INF unsupported" { target pdp11*-*-* vax-*-* } .-1 } */

float fh = __builtin_huge_valf();
double dh = __builtin_huge_val();
long double lh = __builtin_huge_vall();

