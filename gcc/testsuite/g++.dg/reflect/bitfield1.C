// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct A {
  int x : 4, y : 4;
};

constexpr auto f() {
  return &[:^^A::y:];  // { dg-error "invalid pointer to bit-field .A::y." }
}

static_assert(2 == A{1, 2}.*(f()));
