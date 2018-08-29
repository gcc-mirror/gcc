// PR c++/84424
// { dg-do compile { target c++11 } }
// { dg-options "" }

typedef int vec __attribute__ ((vector_size (2 * sizeof (int))));

constexpr vec u = { 1, 2 };
constexpr vec v = __builtin_shuffle (v, u); // { dg-error "" }
