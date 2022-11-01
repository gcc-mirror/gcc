// Excess precision tests.  Verify excess precision doesn't affect
// actual types.
// { dg-do compile { target c++11 } }
// { dg-options "-mfpmath=387 -fexcess-precision=standard" }

namespace std {
  template<typename T, T v> struct integral_constant {
    static constexpr T value = v;
  };
  typedef integral_constant<bool, false> false_type;
  typedef integral_constant<bool, true> true_type;
  template<class T, class U>
  struct is_same : std::false_type {};
  template <class T>
  struct is_same<T, T> : std::true_type {};
}

float f;
double d;

void
test_types (void)
{
#define CHECK_FLOAT(E) static_assert (std::is_same <float, decltype (E)>::value, "")
#define CHECK_DOUBLE(E) static_assert (std::is_same <double, decltype (E)>::value, "")
  CHECK_FLOAT (f + f);
  CHECK_DOUBLE (d + d);
  CHECK_FLOAT (f * f / f);
  CHECK_DOUBLE (d * d / d);
  CHECK_FLOAT (f ? f - f : f);
  CHECK_DOUBLE (d ? d - d : d);
}
