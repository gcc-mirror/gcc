/* { dg-do run } */
/* { dg-add-options ieee } */

_Complex double a = 1;
int main()
{
  _Complex double x = -a / a, y = a / a;
  if (__builtin_copysign (1., __imag__ y)
      != __builtin_copysign (1., __imag__ x))
    __builtin_abort ();
  return 0;
}
