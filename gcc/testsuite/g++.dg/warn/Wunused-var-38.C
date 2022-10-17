// PR c++/80351
// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-variable" }
#include <initializer_list>

constexpr auto int_1 = 1; // { dg-bogus "defined but not used" }
constexpr auto int_2 = 2; // { dg-bogus "defined but not used" }

constexpr auto il_int_1 = {3, 3}; // { dg-bogus "defined but not used" "Triggered by PR80351" }
constexpr auto il_int_2 = {4, 4}; // { dg-bogus "defined but not used" "Not triggered by PR80351" }

constexpr auto il_uint_1 = {5u, 5u}; // { dg-bogus "defined but not used" "Triggered by PR80351" }
constexpr auto il_uint_2 = {6u, 6u}; // { dg-bogus "defined but not used" "Not triggered by PR80351" }

constexpr auto uint_1 = 7u; // { dg-bogus "defined but not used" }
constexpr auto uint_2 = 8u; // { dg-bogus "defined but not used" }
