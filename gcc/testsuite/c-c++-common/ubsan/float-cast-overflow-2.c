/* { dg-do run } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-fsanitize=float-cast-overflow" } */

#include "float-cast.h"

int
main (void)
{
  const double inf = __builtin_inf ();
  const double nan = __builtin_nan ("");
  volatile double d;

  __int128 i;
  d = INT128_MIN;
  CHECK_BOUNDARY (i, d);
  d = 0.0;
  CHECK_BOUNDARY (i, d);
  d = INT128_MAX;
  CHECK_BOUNDARY (i, d);
  CHECK_NONNUMBERS (i);

  unsigned __int128 u;
  d = UINT128_MAX;
  CHECK_BOUNDARY (u, d);
  d = 0.0;
  CHECK_BOUNDARY (u, d);
  CHECK_NONNUMBERS (u);

  return 0;
}

/* { dg-output "runtime error: 1.70141e\\\+38 is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 1.70141e\\\+38 is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 1.70141e\\\+38 is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 1.70141e\\\+38 is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 1.70141e\\\+38 is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 1.70141e\\\+38 is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 1.70141e\\\+38 is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 1.70141e\\\+38 is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 1.70141e\\\+38 is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 1.70141e\\\+38 is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 1.70141e\\\+38 is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 1.70141e\\\+38 is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 1.70141e\\\+38 is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 1.70141e\\\+38 is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 1.70141e\\\+38 is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: nan is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: -?nan is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: inf is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: -inf is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 3.40282e\\\+38 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 3.40282e\\\+38 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 3.40282e\\\+38 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 3.40282e\\\+38 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 3.40282e\\\+38 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 3.40282e\\\+38 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 3.40282e\\\+38 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 3.40282e\\\+38 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 3.40282e\\\+38 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 3.40282e\\\+38 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 3.40282e\\\+38 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 3.40282e\\\+38 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 3.40282e\\\+38 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 3.40282e\\\+38 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: 3.40282e\\\+38 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: -5 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: -1.5 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: -1 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: nan is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: -?nan is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: inf is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*runtime error: -inf is outside the range of representable values of type '__int128 unsigned'" } */
