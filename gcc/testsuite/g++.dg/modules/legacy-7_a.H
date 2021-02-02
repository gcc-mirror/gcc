// { dg-additional-options "-std=c++2a -fmodule-header" }
// { dg-module-cmi {} }

#define throw(...) /* { dg-warning "-:not exporting" } */ \
  noexcept(__VA_OPT__(false))

