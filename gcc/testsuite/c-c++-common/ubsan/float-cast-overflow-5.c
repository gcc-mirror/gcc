/* { dg-do run { target i?86-*-* x86_64-*-* ia64-*-* } } */
/* { dg-options "-fsanitize=float-cast-overflow" } */

#include <limits.h>
#include "float-cast.h"

int
main (void)
{
  volatile __float128 f;

  volatile signed char s;
  f = SCHAR_MIN;
  CHECK_BOUNDARY (s, f);
  f = 0.0q;
  CHECK_BOUNDARY (s, f);
  f = SCHAR_MAX;
  CHECK_BOUNDARY (s, f);

  volatile unsigned char u;
  f = UCHAR_MAX;
  CHECK_BOUNDARY (u, f);
  f = 0.0q;
  CHECK_BOUNDARY (u, f);

  return 0;
}

/* { dg-output " \[^\n\r]* is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* \[^\n\r]* is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* \[^\n\r]* is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* \[^\n\r]* is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* \[^\n\r]* is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* \[^\n\r]* is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* \[^\n\r]* is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* \[^\n\r]* is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* \[^\n\r]* is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* \[^\n\r]* is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* \[^\n\r]* is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* \[^\n\r]* is outside the range of representable values of type" } */
