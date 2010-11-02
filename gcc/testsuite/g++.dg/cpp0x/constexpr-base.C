// Test base/member class and static_assert with constexpr
// { dg-options -std=c++0x }

struct A {
  int i;
  constexpr A(int _i): i(_i) { }
};
struct B: A {
  A a;
  int j;
  constexpr B(int _ib, int _ia, int _j): A(_ib), a(_ia), j(_j) { }
};

constexpr B b (12, 24, 36);

#define SA(X) static_assert (X, #X)
SA(b.i==12 && b.a.i==24 && b.j==36);
