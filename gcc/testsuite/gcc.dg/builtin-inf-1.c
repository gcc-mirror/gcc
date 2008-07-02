/* { dg-do compile } */

float fi = __builtin_inff();
double di = __builtin_inf();
long double li = __builtin_infl();

float fh = __builtin_huge_valf();
double dh = __builtin_huge_val();
long double lh = __builtin_huge_vall();

/* { dg-error "does not support infinity" "INF unsupported" { target vax-*-* spu-*-* } 3 } */
/* { dg-error "does not support infinity" "INF unsupported" { target vax-*-* } 4 } */
/* { dg-error "does not support infinity" "INF unsupported" { target vax-*-* } 5 } */
