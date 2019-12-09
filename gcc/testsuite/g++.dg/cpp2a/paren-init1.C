// PR c++/91363 - P0960R3: Parenthesized initialization of aggregates.
// { dg-do run { target c++2a } }

struct A {
  int i;
  int j;
  int k;
};

struct B {
  int i;
  int j;
  int k = 42;
};

struct C {
  A a;
};

struct D {
  A a1;
  A a2;
};

struct E {
  int i;
};

// F has a base class, but it's not virtual, private, or protected, so this is
// still an aggregate (since C++17).
struct F : E {
  int j;
  int k;
};

F f({1}, 2, 3);

// A non-virtual member function doesn't make it a non-aggregate.
struct G {
  int i;
  double j;
  int foo(int, int);
};

G g(1, 2.14);

class H {
public:
  H (int) { }
};

class I : public H { };

int i;
A a1(1, 2);
A a2(1.0, 2);
A a3(++i, ++i);
const A& ra(1, 2);

A ca = A(1, 2);
A ca2 = A(1.0, 2);
A ca3 = A(++i, ++i);
const A& rca = A(1, 2);

B b1(1, 2, 3);
B b2(1, 2);
B b3(1);

C c1({5, 6, 7});
D d1({1, 2, 3}, {5, 6, 7});

struct W {
  const char *s, *t;
};
W w1("fluffer", "nutter");

struct Y {
  char a[4];
};
Y y("yew");

int
main ()
{
  I(10);

  // A::k will be value-initialized.
  if (a1.i != 1 || a1.j != 2 || a1.k != 0)
    __builtin_abort ();
  if (a2.i != 1 || a2.j != 2 || a2.k != 0)
    __builtin_abort ();
  if (a3.i != 1 || a3.j != 2 || a3.k != 0)
    __builtin_abort ();
  if (ra.i != 1 || ra.j != 2 || ra.k != 0)
    __builtin_abort ();
  if (ca.i != 1 || ca.j != 2 || ca.k != 0)
    __builtin_abort ();
  if (ca2.i != 1 || ca2.j != 2 || ca2.k != 0)
    __builtin_abort ();
  if (ca3.i != 3 || ca3.j != 4 || ca3.k != 0)
    __builtin_abort ();

  if (b1.i != 1 || b1.j != 2 || b1.k != 3)
    __builtin_abort ();
  // The default member initializer will be used for B::k.
  if (b2.i != 1 || b2.j != 2 || b2.k != 42)
    __builtin_abort ();
  if (b3.i != 1 || b3.j != 0 || b3.k != 42)
    __builtin_abort ();

  if (c1.a.i != 5 || c1.a.j != 6 || c1.a.k != 7)
    __builtin_abort ();

  if (f.i != 1 || f.j != 2 || f.k != 3)
    __builtin_abort ();
}
