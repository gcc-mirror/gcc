/* { dg-do compile { target { c || c++14 } } } */
/* { dg-options "-Wdeprecated-octal-literal" } */
/* { dg-additional-options "-std=c23" { target c } } */

int a = 0'0;				/* { dg-warning "octal literals without '0o' or '0O' prefix other than '0' are deprecated in" } */
unsigned long b = 000'000'001UL;	/* { dg-warning "octal literals without '0o' or '0O' prefix other than '0' are deprecated in" } */
#if 0'0U == 0				/* { dg-warning "octal literals without '0o' or '0O' prefix other than '0' are deprecated in" } */
#endif
#if 01'23'45'67ul == 0			/* { dg-warning "octal literals without '0o' or '0O' prefix other than '0' are deprecated in" } */
#endif
