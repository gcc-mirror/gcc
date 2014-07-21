/* { dg-do run { target { lp64 || ilp32 } } } */
/* { dg-options "-fsanitize=float-cast-overflow" } */
/* { dg-additional-options "-msse2 -mfpmath=sse" { target { sse2_runtime && ia32 } } } */

#include <limits.h>
#include "float-cast.h"

int
main (void)
{
  const double inf = __builtin_inf ();
  const double nan = __builtin_nan ("");
  volatile double d;

  volatile signed char sc;
  d = SCHAR_MIN;
  CHECK_BOUNDARY (sc, d);
  d = 0.0;
  CHECK_BOUNDARY (sc, d);
  d = SCHAR_MAX;
  CHECK_BOUNDARY (sc, d);
  CHECK_NONNUMBERS (sc);

  volatile unsigned char uc;
  d = UCHAR_MAX;
  CHECK_BOUNDARY (uc, d);
  d = 0.0;
  CHECK_BOUNDARY (uc, d);
  CHECK_NONNUMBERS (uc);

  volatile short int s;
  d = SHRT_MIN;
  CHECK_BOUNDARY (s, d);
  d = 0.0;
  CHECK_BOUNDARY (s, d);
  d = SHRT_MAX;
  CHECK_BOUNDARY (s, d);
  CHECK_NONNUMBERS (s);

  volatile unsigned short int us;
  d = USHRT_MAX;
  CHECK_BOUNDARY (us, d);
  d = 0.0;
  CHECK_BOUNDARY (us, d);
  CHECK_NONNUMBERS (us);

  volatile int i;
  d = INT_MIN;
  CHECK_BOUNDARY (i, d);
  d = 0.0;
  CHECK_BOUNDARY (i, d);
  d = INT_MAX;
  CHECK_BOUNDARY (i, d);
  CHECK_NONNUMBERS (i);

  volatile unsigned int u;
  d = UINT_MAX;
  CHECK_BOUNDARY (u, d);
  d = 0.0;
  CHECK_BOUNDARY (u, d);
  CHECK_NONNUMBERS (u);

  volatile long l;
  /* 64-bit vs 32-bit longs matter causes too much of a headache.  */
  d = 0.0;
  CHECK_BOUNDARY (l, d);
  CHECK_NONNUMBERS (l);

  volatile unsigned long ul;
  d = 0.0;
  CHECK_BOUNDARY (ul, d);
  CHECK_NONNUMBERS (ul);

  volatile long long ll;
  d = LLONG_MIN;
  CHECK_BOUNDARY (ll, d);
  d = 0.0;
  CHECK_BOUNDARY (ll, d);
  d = LLONG_MAX;
  CHECK_BOUNDARY (ll, d);
  CHECK_NONNUMBERS (ll);

  volatile unsigned long long ull;
  d = ULLONG_MAX;
  CHECK_BOUNDARY (ull, d);
  d = 0.0;
  CHECK_BOUNDARY (ull, d);
  CHECK_NONNUMBERS (ull);

  return 0;
}

/* { dg-output "value -133 is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -129.5 is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -129 is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 128 is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 128.5 is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 132 is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value nan is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -?nan is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value inf is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -inf is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 256 is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 256.5 is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 260 is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -5 is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1.5 is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value nan is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -?nan is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value inf is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -inf is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -32773 is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -32769.5 is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -32769 is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 32768 is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 32768.5 is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 32772 is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value nan is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -?nan is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value inf is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -inf is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 65536 is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 65536.5 is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 65540 is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -5 is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1.5 is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value nan is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -?nan is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value inf is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -inf is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -2.14748e\\\+09 is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -2.14748e\\\+09 is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -2.14748e\\\+09 is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 2.14748e\\\+09 is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 2.14748e\\\+09 is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 2.14748e\\\+09 is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value nan is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -?nan is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value inf is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -inf is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 4.29497e\\\+09 is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 4.29497e\\\+09 is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 4.29497e\\\+09 is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -5 is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1.5 is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value nan is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -?nan is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value inf is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -inf is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value nan is outside the range of representable values of type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -?nan is outside the range of representable values of type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value inf is outside the range of representable values of type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -inf is outside the range of representable values of type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -5 is outside the range of representable values of type 'long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1.5 is outside the range of representable values of type 'long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value nan is outside the range of representable values of type 'long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -?nan is outside the range of representable values of type 'long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value inf is outside the range of representable values of type 'long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -inf is outside the range of representable values of type 'long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 9.22337e\\\+18 is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 9.22337e\\\+18 is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 9.22337e\\\+18 is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 9.22337e\\\+18 is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 9.22337e\\\+18 is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 9.22337e\\\+18 is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 9.22337e\\\+18 is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 9.22337e\\\+18 is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 9.22337e\\\+18 is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 9.22337e\\\+18 is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 9.22337e\\\+18 is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 9.22337e\\\+18 is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 9.22337e\\\+18 is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 9.22337e\\\+18 is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 9.22337e\\\+18 is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value nan is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -?nan is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value inf is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -inf is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.84467e\\\+19 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.84467e\\\+19 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.84467e\\\+19 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.84467e\\\+19 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.84467e\\\+19 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.84467e\\\+19 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.84467e\\\+19 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.84467e\\\+19 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.84467e\\\+19 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.84467e\\\+19 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.84467e\\\+19 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.84467e\\\+19 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.84467e\\\+19 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.84467e\\\+19 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.84467e\\\+19 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -5 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1.5 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value nan is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -?nan is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value inf is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -inf is outside the range of representable values of type 'long long unsigned int'" } */
