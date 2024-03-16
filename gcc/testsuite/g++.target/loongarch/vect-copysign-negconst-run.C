/* { dg-do run } */
/* { dg-options "-O2 -march=loongarch64 -mlasx -mno-strict-align" } */
/* { dg-require-effective-target loongarch_asx_hw } */

#include "vect-copysign-negconst.C"

double d[] = {1.2, -3.4, -5.6, 7.8};
float f[] = {1.2, -3.4, -5.6, 7.8, -9.0, -11.4, 51.4, 1919.810};

double _abs(double x) { return __builtin_fabs (x); }
float _abs(float x) { return __builtin_fabsf (x); }

template <class T>
void
check (T *arr, T *orig, int len)
{
  for (int i = 0; i < len; i++)
    {
      if (arr[i] > 0)
	__builtin_trap ();
      if (_abs (arr[i]) != _abs (orig[i]))
	__builtin_trap ();
    }
}

int
main()
{
  double test_d[4];
  float test_f[8];

  __builtin_memcpy (test_d, d, sizeof (test_d));
  force_negative<2> (test_d);
  check (test_d, d, 2);

  __builtin_memcpy (test_d, d, sizeof (test_d));
  force_negative<4> (test_d);
  check (test_d, d, 4);

  __builtin_memcpy (test_f, f, sizeof (test_f));
  force_negative<4> (test_f);
  check (test_f, f, 4);

  __builtin_memcpy (test_f, f, sizeof (test_f));
  force_negative<8> (test_f);
  check (test_f, f, 8);
}
