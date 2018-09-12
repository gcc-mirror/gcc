// { dg-additional-options "-std=c++2a -fmodule-legacy=\"legacy-7_a.H\"" }
// { dg-module-bmi {"legacy-7_a.H"} }

#define throw(...) /* { dg-warning "-:not exporting" } */ \
  noexcept(__VA_OPT__(false))

