/* { dg-do run } */
/* { dg-options "-ffinite-math-only" } */
/* { dg-add-options ieee } */

double a = 0.0;
double b = -0.0;

int main()
{
  double min1 = a < b ? a : b;
  double max1 = a > b ? a : b;
  double min2 = b < a ? b : a;
  double max2 = b > a ? b : a;
  if (__builtin_copysign (1., min1) != -1.
      || __builtin_copysign (1., max1) != -1.
      || __builtin_copysign (1., min2) != 1.
      || __builtin_copysign (1., max2) != 1.)
    __builtin_abort ();
  return 0;
}
