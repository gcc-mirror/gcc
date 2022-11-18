// DR 2583 - Common initial sequence should consider over-alignment.
// { dg-do compile { target c++11 } }

#include <type_traits>

struct A {
  int i;
  char c;
};

struct B {
  int i;
  alignas(8) char c;
};

struct C {
  int i;
  alignas(alignof(char)) char c;
};

struct D {
  alignas(alignof(int)) int i;
  char c;
};

struct S0 {
  alignas(16) char x[128];
  int i;
};

struct alignas(16) S1 {
  char x[128];
  int i;
};

#if __cpp_lib_is_layout_compatible >= 201907L
static_assert (std::is_corresponding_member (&A::i, &B::i), "");
static_assert (std::is_corresponding_member (&A::c, &B::c) == (alignof (char) == 8), "");
static_assert (std::is_corresponding_member (&A::i, &C::i), "");
static_assert (std::is_corresponding_member (&A::c, &C::c), "");
static_assert (std::is_corresponding_member (&A::i, &D::i), "");
static_assert (std::is_corresponding_member (&A::c, &D::c), "");
static_assert (std::is_corresponding_member (&S0::x, &S1::x) == (alignof (char) == 16), "");
static_assert (std::is_corresponding_member (&S0::i, &S1::i) == (alignof (char) == 16), "");
#endif
