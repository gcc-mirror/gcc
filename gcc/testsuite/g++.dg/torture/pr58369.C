// { dg-do compile }
// { dg-additional-options "-Wno-return-type" }
// Reduced from boost-1.54

int pow(int, int);
int sqrt(int);

class PolicyA { };

template <class>
int max_value() { return 0x7fffffff; }

template <class>
int min_value() { return 1; }

void raise_denorm_error();

template <class T>
void raise_domain_error(int, int, const T &, const PolicyA &);

template <class>
int check_overflow(long double p1) {
  long double __trans_tmp_2 = __builtin_fabsl(p1);
  if (__trans_tmp_2 > max_value<int>())
    return 1;
  return 0;
}

template <class>
int check_underflow(long double p1) {
  if (p1 && (double)p1)
    return 1;
  return 0;
}

template <class>
int check_denorm(long double p1) {
  long double __trans_tmp_3 = __builtin_fabsl(p1);
  if (__trans_tmp_3 < min_value<int>() && (double)p1) {
    raise_denorm_error();
    return 1;
  }
  return 0;
}

template <class, class>
double checked_narrowing_cast(long double p1) {
  if (check_overflow<int>(p1))
    return 0;
  if (check_underflow<int>(p1))
    return 0;
  if (check_denorm<int>(p1))
    return 0;
  return (double)p1;
}

long double ellint_rf_imp(long double, long double, long double);

template <typename T, typename Policy>
T ellint_rj_imp(T p1, T p2, T p3, T p4, Policy &p5) {
  T value, tolerance, P, S3;
  if (p4)
    return 0;
  if (p3 || p1)
    raise_domain_error(0, 0, 0, p5);
  tolerance = pow(0, 0);
  if (p4) {
    T q = -p4;
    {
      long double q6 = ellint_rj_imp((long double)p1, (long double)(double)p2, (long double)(double)p3, (long double)(int)0, p5);
      value = checked_narrowing_cast<T, int>(q6);
    }
    {
      long double q7 = ellint_rf_imp((long double)p1, (long double)(double)p2, (long double)(double)p3);
      value -= checked_narrowing_cast<T, const int>(q7);
    }
    value += p1 * p3 + p4 * q;
    return value;
  }
  do {
    P = p4 / p1;
    if (0 < tolerance)
      break;
    sqrt(p3);
  } while (1);
  S3 = P * p2 * 0;
  value = S3 / p1;
  return value;
}

template <typename Policy>
void ellint_pi_imp4(double, double p3, Policy &p4) {
  double x, y, z;
  ellint_rj_imp(x, y, z, p3, p4);
}

template <typename Policy>
double ellint_pi_imp5(double, double p3, double p4, Policy &p5) {
  double x, y, z, p;
  if (p3 > 0)
    return 0;
  ellint_rj_imp(x, y, z, p, p5);
  ellint_pi_imp4((double)0, p4, p5);
}

void boost_ellint_3f() {
  PolicyA p4;
  ellint_pi_imp5((double)0, (double)0, (double)0, p4);
}

