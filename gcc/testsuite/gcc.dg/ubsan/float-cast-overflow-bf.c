/* { dg-do run } */
/* { dg-options "-fsanitize=float-cast-overflow" } */

struct
{
  int i:1;
} s;

struct
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
  (VAR) = (VAL) - 0.5;                  \
  (VAR) = (VAL) - 0.0000001;            \
  (VAR) = (VAL) - 0.0;                  \
  (VAR) = (VAL);                        \
  (VAR) = (VAL) + 0.0;                  \
  (VAR) = (VAL) + 0.0000001;            \
  (VAR) = (VAL) + 0.5;                  \
  (VAR) = (VAL) + 1.0;                  \
  (VAR) = (VAL) + 1.5;

  /* Signed bit-field.  (-1, 0) is valid.  */
  d = -1.0;
  CHECK_BOUNDARY (s.i, d);
  d = 0.0;
  CHECK_BOUNDARY (s.i, d);
  d = 1.0;
  CHECK_BOUNDARY (s.i, d);

  /* Unsigned bit-field.  (0, 1) is valid.  */
  d = -1.0;
  CHECK_BOUNDARY (t.i, d);
  d = 0.0;
  CHECK_BOUNDARY (t.i, d);
  d = 1.0;
  CHECK_BOUNDARY (t.i, d);

  return 0;
}

/* { dg-output "value -2.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -2 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 1.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 2 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 2.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -2.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -2 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 2 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 2.5 is outside the range of representable values of type\[^\n\r]*(\n|\r\n|\r)" } */
