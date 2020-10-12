// PR c++/97145
// { dg-do compile { target c++11 } }
// { dg-options "-fsanitize=address,pointer-subtract,pointer-compare" }

constexpr char *a = nullptr;
constexpr auto b = a - a;
constexpr auto c = a < a;
