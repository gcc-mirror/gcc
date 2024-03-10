// { dg-require-effective-target c++11 }

#include <map>

std::array<int, 10> a1; /* { dg-error "incomplete type" } */
/* { dg-message "'std::array' is defined in header '<array>'; this is probably fixable by adding '#include <array>'" "hint" { target *-*-* } .-1 } */

std::array<int, 10> a2 {5}; /* { dg-error "incomplete type" } */
/* { dg-message "'std::array' is defined in header '<array>'; this is probably fixable by adding '#include <array>'" "hint" { target *-*-* } .-1 } */

