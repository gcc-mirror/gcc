// PR c++/89285
// { dg-do compile { target c++14 } }

struct A {
  int a {};
};
struct B {
  int b {};
  constexpr B (A *x) {
    int *c = &x->a;
    while (*c)
      c = reinterpret_cast<int *>((reinterpret_cast<char *>(c) + *c));
    *c = reinterpret_cast<char *>(this) - reinterpret_cast<char *>(c);
  }
};
struct C : A {
  B bar {this};
};

C foo {};
