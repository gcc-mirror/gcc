/* { dg-do run { target { { { i?86-*-* x86_64-*-* } && { ! { ia32 } } || { ia64-*-* } } } } } */
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

/* { dg-output " -133 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* -129.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* -129 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* 128 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* 128.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* 132 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* 256 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* 256.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* 260 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* -5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* -1.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* -1 is outside the range of representable values of type" } */
