/* { dg-do compile } */
/* { dg-options "-Winline -O2" } */
#include <stdarg.h>
inline __attribute__ ((always_inline)) void
e(int t, ...)
{				/* { dg-message "sorry\[^\n\]*variable argument" "" } */
  va_list q;
  va_start (q, t);
}
