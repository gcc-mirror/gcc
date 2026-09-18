// P3899R3 - Clarify the behavior of floating-point overflow
// { dg-do compile { target { c++11 && flt_dbl_ldbl_inf_nan } } }
// { dg-options "" }

constexpr float minf = __FLT_MIN__;
constexpr float maxf = __FLT_MAX__;
constexpr float inff = __builtin_inff ();
constexpr float nanf = __builtin_nanf ("");
constexpr float snanf = __builtin_nansf ("");

constexpr float inf2f = inff * 2.0f; // OK, also positive infinity
constexpr float zerof = minf / maxf; // OK, result cannot be represented, and is rounded to zero
constexpr float oflof = maxf * 2.0f; // { dg-error "is not a constant expression" }
constexpr float nan2f = nanf * 2.0f; // OK, propagating a NaN
constexpr float udeff = inff * 0.0f; // { dg-error "is not a constant expression" }
constexpr float div0f = maxf / 0.0f; // { dg-error "is not a constant expression" }
constexpr float zero2f = minf * minf;
constexpr bool nancmpf = nanf < 0.0f;
constexpr float snan2f = snanf * 2.0f;
constexpr float div02f = 1.0f / 0.0f; // { dg-error "is not a constant expression" }
constexpr float div03f = 0.0f / 0.0f; // { dg-error "is not a constant expression" }

constexpr double mind = __DBL_MIN__;
constexpr double maxd = __DBL_MAX__;
constexpr double infd = __builtin_inf ();
constexpr double nand = __builtin_nan ("");
constexpr double snand = __builtin_nans ("");

constexpr double inf2d = infd * 2.0; // OK, also positive infinity
constexpr double zerod = mind / maxd; // OK, result cannot be represented, and is rounded to zero
constexpr double oflod = maxd * 2.0; // { dg-error "is not a constant expression" }
constexpr double nan2d = nand * 2.0; // OK, propagating a NaN
constexpr double udefd = infd * 0.0; // { dg-error "is not a constant expression" }
constexpr double div0d = maxd / 0.0; // { dg-error "is not a constant expression" }
constexpr double zero2d = mind * mind;
constexpr bool nancmpd = nand < 0.0;
constexpr double snan2d = snand * 2.0;
constexpr double div02d = 1.0 / 0.0; // { dg-error "is not a constant expression" }
constexpr double div03d = 0.0 / 0.0; // { dg-error "is not a constant expression" }

constexpr long double minld = __LDBL_MIN__;
constexpr long double maxld = __LDBL_MAX__;
constexpr long double infld = __builtin_infl ();
constexpr long double nanld = __builtin_nanl ("");
constexpr long double snanld = __builtin_nansl ("");

constexpr long double inf2ld = infld * 2.0L; // OK, also positive infinity
constexpr long double zerold = minld / maxld; // OK, result cannot be represented, and is rounded to zero
					      // { dg-bogus "is not a constant expression" "" { xfail long_double_is_ibm128 } .-1 }
constexpr long double oflold = maxld * 2.0L; // { dg-error "is not a constant expression" }
constexpr long double nan2ld = nanld * 2.0L; // OK, propagating a NaN
constexpr long double udefld = infld * 0.0L; // { dg-error "is not a constant expression" }
constexpr long double div0ld = maxld / 0.0L; // { dg-error "is not a constant expression" }
constexpr long double zero2ld = minld * minld; // { dg-bogus "is not a constant expression" "" { xfail long_double_is_ibm128 } }
constexpr bool nancmpld = nanld < 0.0L;
constexpr long double snan2ld = snanld * 2.0L;
constexpr long double div02ld = 1.0L / 0.0L; // { dg-error "is not a constant expression" }
constexpr long double div03ld = 0.0L / 0.0L; // { dg-error "is not a constant expression" }
