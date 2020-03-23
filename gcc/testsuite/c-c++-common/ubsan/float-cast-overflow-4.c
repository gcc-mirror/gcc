/* { dg-do run { target { lp64 } } } */
/* { dg-options "-fsanitize=float-cast-overflow -Wno-overflow" } */

#include <limits.h>
#include "float-cast.h"

int
main (void)
{
  const long double inf = __builtin_infl ();
  const long double nan = __builtin_nanl ("");
  volatile long double ld;

  volatile int i;
  ld = INT_MIN;
  CHECK_BOUNDARY (i, ld);
  ld = 0.0l;
  CHECK_BOUNDARY (i, ld);
  ld = INT_MAX;
  CHECK_BOUNDARY (i, ld);
  CHECK_NONNUMBERS (i);

  volatile unsigned int u;
  ld = UINT_MAX;
  CHECK_BOUNDARY (u, ld);
  ld = 0.0l;
  CHECK_BOUNDARY (u, ld);
  CHECK_NONNUMBERS (u);

  return 0;
}

/* { dg-output " -2.14748e\\\+09 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* -2.14748e\\\+09 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* -2.14748e\\\+09 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* 2.14748e\\\+09 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* 2.14748e\\\+09 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* 2.14748e\\\+09 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* nan is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* -?nan is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* inf is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* -inf is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* 4.29497e\\\+09 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* 4.29497e\\\+09 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* 4.29497e\\\+09 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* -5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* -1.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* -1 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* nan is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* -?nan is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* inf is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]* -inf is outside the range of representable values of type" } */
