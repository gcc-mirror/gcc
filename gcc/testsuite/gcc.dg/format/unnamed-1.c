/* Test for warnings with possibly unnamed integer types.  Bug 24329.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */
/* { dg-options "-Wformat -msse" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-require-effective-target sse { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */


#include "format.h"

/* Definition of TItype follows same logic as in gcc.dg/titype-1.c,
   but must be a #define to avoid giving the type a name.  */
#if defined(__LP64__) && !defined(__hppa__)
#define TItype int __attribute__ ((mode (TI)))
#else
#define TItype long
#endif

void
f (TItype x)
{
  printf("%d", x); /* { dg-warning "expects type" } */
  printf("%d", 141592653589793238462643383279502884197169399375105820974944); /* { dg-warning "expects type" } */
  /* { dg-warning "unsigned only|too large" "constant" { target *-*-* } 23 } */
}
