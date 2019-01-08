// PR c++/88744
// { dg-do compile { target c++2a } }

#define SA(X) static_assert((X),#X)

struct S {
  int a;
  int b;
  constexpr S(int a_, int b_) : a{a_}, b{b_} { }
};

template<S s = {1, 2}>
struct X {
  static constexpr int i = s.a;
  static constexpr int j = s.b;
};
X x; // ok, X<{1, 2}>
X<{3, 4}> x2;

SA (x.i == 1);
SA (x.j == 2);
SA (x2.i == 3);
SA (x2.j == 4);
