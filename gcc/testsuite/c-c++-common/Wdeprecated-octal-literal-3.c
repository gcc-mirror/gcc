/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-additional-options "-std=c2y" { target c } } */

int a = 0;
int b = 00;	/* { dg-warning "octal literals without '0o' or '0O' prefix other than '0' are deprecated in" "" { target c++29 } } */
unsigned long long d = 042ULL;	/* { dg-warning "octal literals without '0o' or '0O' prefix other than '0' are deprecated in" "" { target c++29 } } */
#if 0U == 0
#endif
#if 00l == 0	/* { dg-warning "octal literals without '0o' or '0O' prefix other than '0' are deprecated in" "" { target c++29 } } */
#endif
#if 042 == 0	/* { dg-warning "octal literals without '0o' or '0O' prefix other than '0' are deprecated in" "" { target c++29 } } */
#endif
