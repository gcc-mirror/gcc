// PR c++/93618
// { dg-do compile { target c++11 } }
// { dg-options "" }

template <typename T>
struct C {
  ~C () = default;
  T *p = nullptr;
};

class A {
  struct B {
    int c;
    C<B*> d[];
  };
  void foo (int f) { B s; s.c = f; }
  B e;
};
