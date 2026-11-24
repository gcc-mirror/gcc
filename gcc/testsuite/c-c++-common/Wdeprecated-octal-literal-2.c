/* { dg-do compile } */
/* { dg-options "-Wno-deprecated-octal-literal" } */
/* { dg-additional-options "-std=c23" { target c } } */

int a = 0;
int b = 00;
unsigned long long d = 042ULL;
#if 0U == 0
#endif
#if 00l == 0
#endif
#if 042 == 0
#endif
