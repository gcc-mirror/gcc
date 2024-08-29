// PR c++/116416
// { dg-do compile { target c++11 } }
// { dg-options "-O" }

struct optional {
  constexpr optional(int) {}
};
optional foo() { return 2; }


struct C {
  constexpr C(int) {}
};

struct B {
  C fn(int) { return 0; }
};

void
g ()
{
  B b;
  b.fn(0);
}
