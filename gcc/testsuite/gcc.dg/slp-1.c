/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model" } */

struct R {
  double d1;
  double d2;
};

struct R foo (struct R arg)
{
  struct R ret;
  ret.d1 = arg.d2 * (0.0 - arg.d1);
  ret.d2 = ret.d1;
  return ret;
}
