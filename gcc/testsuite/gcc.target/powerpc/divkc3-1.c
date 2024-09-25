/* { dg-do run { target { powerpc64*-*-* && p8vector_hw } } } */
/* { dg-options "-mfloat128 -mvsx" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

void abort ();

typedef __complex float __cfloat128 __attribute__((mode(KC)));

__cfloat128 divide (__cfloat128 x, __cfloat128 y)
{
  return x / y;
}

__cfloat128 z, a;

int main ()
{
  z = divide (5.0q + 5.0jq, 2.0q + 1.0jq);
  a = 3.0q + 1.0jq;
  if (z != a)
    abort ();
  return 0;
}
