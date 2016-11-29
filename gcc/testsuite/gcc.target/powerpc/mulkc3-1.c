/* { dg-do run { target { powerpc64*-*-* && p8vector_hw } } } */
/* { dg-options "-mfloat128 -mvsx" } */

void abort ();

typedef __complex float __cfloat128 __attribute__((mode(KC)));

__cfloat128 multiply (__cfloat128 x, __cfloat128 y)
{
  return x * y;
}

__cfloat128 z, a;

int main ()
{
  z = multiply (2.0q + 1.0jq, 3.0q + 1.0jq);
  a = 5.0q + 5.0jq;
  if (z != a)
    abort ();
  return 0;
}
