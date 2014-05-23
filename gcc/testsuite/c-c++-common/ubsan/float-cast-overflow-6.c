/* { dg-do run { target { { x86_64-*-* ia64-*-* } && { ! { ia32 } } } } } */
/* { dg-options "-fsanitize=float-cast-overflow" } */

#include <limits.h>
#include "float-cast.h"

int
main (void)
{
  volatile __float80 f;

  volatile signed char s;
  f = SCHAR_MIN;
  CHECK_BOUNDARY (s, f);
  f = 0.0w;
  CHECK_BOUNDARY (s, f);
  f = SCHAR_MAX;
  CHECK_BOUNDARY (s, f);

  volatile unsigned char u;
  f = UCHAR_MAX;
  CHECK_BOUNDARY (u, f);
  f = 0.0w;
  CHECK_BOUNDARY (u, f);

  return 0;
}

/* { dg-output "value -133 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -129.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -129 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 128 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 128.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 132 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 256 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 256.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 260 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
