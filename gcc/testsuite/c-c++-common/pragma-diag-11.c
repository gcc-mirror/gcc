/* { dg-do compile } */
/* { dg-options "-Wundef" } */
#pragma GCC diagnostic ignored "-Wundef"
#if FOO
#endif
#define P _Pragma ("GCC diagnostic push") _Pragma ("GCC diagnostic warning \"-Wundef\"")
P
#if FOO2 /* { dg-warning "is not defined" } */
#endif
#pragma GCC diagnostic pop
#if FOO3
#endif
int i;
