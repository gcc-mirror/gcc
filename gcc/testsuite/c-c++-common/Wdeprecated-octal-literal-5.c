/* { dg-do compile { target { c || c++14 } } } */
/* { dg-options "-Wno-deprecated-octal-literal" } */
/* { dg-additional-options "-std=c23" { target c } } */

int a = 0'0;
unsigned long b = 000'000'001UL;
#if 0'0U == 0
#endif
#if 01'23'45'67ul == 0
#endif
