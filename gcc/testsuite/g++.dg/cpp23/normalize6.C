// P1949R7
// { dg-do compile { target c++11 } }
// { dg-options "" }

constexpr int À = 1; // U+00C0
constexpr int À = 2; // U+0041 U+0300 { dg-warning "is not in NFC" }
constexpr int gv1 = \u00c0;
constexpr int gv2 = A\u0300; // { dg-warning "is not in NFC" }
static_assert(gv1 == 1, "");
static_assert(gv2 == 2, "");
