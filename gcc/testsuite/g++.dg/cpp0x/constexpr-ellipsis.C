// { dg-do compile { target c++11 } }
constexpr int ellipsis(...) { return 1; }

constexpr int ellipsis_c = ellipsis(); // OK
constexpr int ellipsis_c2 = ellipsis(42); // Internal error
