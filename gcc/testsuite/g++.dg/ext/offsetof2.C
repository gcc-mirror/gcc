// PR c++/85662
// { dg-do compile { target c++11 } }

struct S { unsigned long x[31]; };
struct T { bool b; S f; };
static_assert (__builtin_offsetof (T, f.x[31 - 1]) == __builtin_offsetof (T, f.x[30]), "");
