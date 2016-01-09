// PR c++/69164
// { dg-do compile { target c++11 } }
// { dg-options "-O2" }

struct A {
  struct B {
    B () {}
    bool : 1;
  };
  B foo () { B r; return r; }
};

struct C {
  struct D {
    D (C *x) : d (x->c.foo ()) {}
    A::B d;
  };
  A c;
};

struct F : C {
  D f = this;
  F (int, int) {}
};

void
bar (int a, int b)
{
  F (b, a);
}
