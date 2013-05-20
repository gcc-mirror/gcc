#include <stdlib.h>
#define N 16

FTYPE input1[N] =
{2.0, 4.0, 8.0, 16.0,
 2.125, 4.25, 8.5, 17.0,
 -2.0, -4.0, -8.0, -16.0,
 -2.125, -4.25, -8.5, -17.0};

FTYPE input2[N] =
{-2.0, 4.0, -8.0, 16.0,
 2.125, -4.25, 8.5, -17.0,
 2.0, -4.0, 8.0, -16.0,
 -2.125, 4.25, -8.5, 17.0};

/* Float comparisons, float results.  */

void
foo (FTYPE *in1, FTYPE *in2, FTYPE *output)
{
  int i = 0;
  /* Vectorizable.  */
  for (i = 0; i < N; i++)
    output[i] = (in1[i] OP in2[i]) ? 2.0 : 4.0;
}

void
bar (FTYPE *in1, FTYPE *in2, FTYPE *output)
{
  int i = 0;
  /* Vectorizable.  */
  for (i = 0; i < N; i++)
    output[i] = (in1[i] INV_OP in2[i]) ? 4.0 : 2.0;
}

void
foobar (FTYPE *in1, FTYPE *in2, FTYPE *output)
{
  int i = 0;
  /* Vectorizable.  */
  for (i = 0; i < N; i++)
    output[i] = (in1[i] OP 0.0) ? 4.0 : 2.0;
}

void
foobarbar (FTYPE *in1, FTYPE *in2, FTYPE *output)
{
  int i = 0;
  /* Vectorizable.  */
  for (i = 0; i < N; i++)
    output[i] = (in1[i] INV_OP 0.0) ? 4.0 : 2.0;
}

/* Float comparisons, int results.  */

void
foo_int (FTYPE *in1, FTYPE *in2, ITYPE *output)
{
  int i = 0;
  /* Vectorizable.  */
  for (i = 0; i < N; i++)
    output[i] = (in1[i] OP in2[i]) ? 2 : 4;
}

void
bar_int (FTYPE *in1, FTYPE *in2, ITYPE *output)
{
  int i = 0;
  /* Vectorizable.  */
  for (i = 0; i < N; i++)
    output[i] = (in1[i] INV_OP in2[i]) ? 4 : 2;
}

void
foobar_int (FTYPE *in1, FTYPE *in2, ITYPE *output)
{
  int i = 0;
  /* Vectorizable.  */
  for (i = 0; i < N; i++)
    output[i] = (in1[i] OP 0.0) ? 4 : 2;
}

void
foobarbar_int (FTYPE *in1, FTYPE *in2, ITYPE *output)
{
  int i = 0;
  /* Vectorizable.  */
  for (i = 0; i < N; i++)
    output[i] = (in1[i] INV_OP 0.0) ? 4 : 2;
}

int
main (int argc, char **argv)
{
  FTYPE out1[N];
  FTYPE out2[N];
  ITYPE outi1[N];
  ITYPE outi2[N];

  int i = 0;
  foo (input1, input2, out1);
  bar (input1, input2, out2);
  for (i = 0; i < N; i++)
    if (out1[i] != out2[i])
      abort ();
  foobar (input1, input2, out1);
  foobarbar (input1, input2, out2);
  for (i = 0; i < N; i++)
    if (out1[i] == out2[i])
      abort ();

  foo_int (input1, input2, outi1);
  bar_int (input1, input2, outi2);
  for (i = 0; i < N; i++)
    if (outi1[i] != outi2[i])
      abort ();
  foobar_int (input1, input2, outi1);
  foobarbar_int (input1, input2, outi2);
  for (i = 0; i < N; i++)
    if (outi1[i] == outi2[i])
      abort ();
  return 0;
}

