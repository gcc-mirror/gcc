/* { dg-do compile { target { c || c++14 } } } */
/* { dg-options "" } */
/* { dg-additional-options "-std=c2y" { target c } } */

int a = 0'0;				/* { dg-warning "octal literals without '0o' or '0O' prefix other than '0' are deprecated in" "" { target c++29 } } */
unsigned long b = 000'000'001UL;	/* { dg-warning "octal literals without '0o' or '0O' prefix other than '0' are deprecated in" "" { target c++29 } } */
#if 0'0U == 0				/* { dg-warning "octal literals without '0o' or '0O' prefix other than '0' are deprecated in" "" { target c++29 } } */
#endif
#if 01'23'45'67ul == 0			/* { dg-warning "octal literals without '0o' or '0O' prefix other than '0' are deprecated in" "" { target c++29 } } */
#endif
