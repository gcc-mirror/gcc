/* { dg-options "-Ofast -mlow-precision-div" } */
/* { dg-do compile } */

float
f1 (float x, float y)
{
  return y / x;
}

/* { dg-final { scan-assembler {\tfrecpe\t(s[0-9]+), s0\n\tfrecps\t(s[0-9]+), \1, s0\n\tfmul\t\1, \1, s1\n\tfmul\ts0, \1, \2\n} } } */

double
f2 (double x, double y)
{
  return y / x;
}

/* { dg-final { scan-assembler {\tfrecpe\t(d[0-9]+), d0\n\tfrecps\t(d[0-9]+), \1, d0\n\tfmul\t\1, \1, \2\n\tfrecps\t\2, \1, d0\n\tfmul\t\1, \1, d1\n\tfmul\td0, \1, \2\n} } } */
