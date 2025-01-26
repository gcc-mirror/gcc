#define SIGNBIT(T, U) \
  int signbit_##U (T x) { return __builtin_signbit (x); }

SIGNBIT (long double, long_double)
SIGNBIT (_Decimal128, _Decimal128)

#define ISFINITE(T, U) \
  int isfinite_##U (T x) { return __builtin_isfinite (x); }

ISFINITE (long double, long_double)
ISFINITE (_Decimal128, _Decimal128)

#define ISINF(T, U) \
  int isinf_##U (T x) { return __builtin_isinf (x); }

ISINF (long double, long_double)
ISINF (_Decimal128, _Decimal128)

#define ISNORMAL(T, U) \
  int isnormal_##U (T x) { return __builtin_isnormal (x); }

ISNORMAL (long double, long_double)
ISNORMAL (_Decimal128, _Decimal128)
