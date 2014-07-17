// { dg-do run { target { int32 } } }
// { dg-options "-fsanitize=float-cast-overflow" }

#define INT_MAX __INT_MAX__
#define INT_MIN (-__INT_MAX__ - 1)
#define UINT_MAX 2 * (unsigned) __INT_MAX__ + 1

struct S
{
  int i:1;
} s;

struct T
{
  unsigned int i:1;
} t;

int
main (void)
{
  volatile double d;

#define CHECK_BOUNDARY(VAR, VAL)        \
  (VAR) = (VAL) - 1.5;                  \
  (VAR) = (VAL) - 1.0;                  \
  (VAR) = (VAL) - 0.75;                  \
  (VAR) = (VAL) - 0.5;                  \
  (VAR) = (VAL) - 0.0000001;            \
  (VAR) = (VAL) - 0.0;                  \
  (VAR) = (VAL);                        \
  (VAR) = (VAL) + 0.0;                  \
  (VAR) = (VAL) + 0.0000001;            \
  (VAR) = (VAL) + 0.5;                  \
  (VAR) = (VAL) + 0.75;                  \
  (VAR) = (VAL) + 1.0;                  \
  (VAR) = (VAL) + 1.5;

  /* Signed bit-field.  (INT_MIN, INT_MAX) is valid.  */
  d = INT_MIN;
  CHECK_BOUNDARY (s.i, d);
  d = 0.0;
  CHECK_BOUNDARY (s.i, d);
  d = INT_MAX;
  CHECK_BOUNDARY (s.i, d);

  /* Unsigned bit-field.  (0, UINT_MAX) is valid.  */
  d = UINT_MAX;
  CHECK_BOUNDARY (t.i, d);
  d = 0.0;
  CHECK_BOUNDARY (t.i, d);

  return 0;
}

/* { dg-output "value -2.14748e\\\+09 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -2.14748e\\\+09 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 2.14748e\\\+09 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 2.14748e\\\+09 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 4.29497e\\\+09 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 4.29497e\\\+09 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type" } */
