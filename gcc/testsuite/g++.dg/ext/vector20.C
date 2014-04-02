/* { dg-do compile { target c++11 } } */

typedef long vec __attribute__((vector_size (2 * sizeof (long))));
constexpr vec v = { 3, 4 };
constexpr vec s = v + v;
constexpr vec w = __builtin_shuffle (v, v);
