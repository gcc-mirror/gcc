/* { dg-do compile } */
/* { dg-options "-Winline -O2" } */
#include <stdarg.h>
inline __attribute__ ((always_inline)) void t(void); /* { dg-error "body not available" "" } */
void
q(void)
{
  t(); 				/* { dg-error "called from here" "" } */
}
inline __attribute__ ((always_inline)) void
q2(void)
{ 				/* { dg-error "recursive" "" } */
  q2(); 			/* { dg-error "called from here" "" } */
  q2(); 			/* { dg-error "called from here" "" } */
}
inline __attribute__ ((always_inline)) void
e(int t, ...)
{				/* { dg-error "variable argument" "" } */
  va_list q;
  va_start (q, t);
}
