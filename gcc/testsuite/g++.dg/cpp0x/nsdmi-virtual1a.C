// PR c++/80431
// { dg-do run { target c++11 } }

// A variant of nsdmi-virtual1.C where A is only a morally virtual base of B.

struct A
{
  A(): i(42) { }
  int i;
  int f() { return i; }
};

struct D : A { int pad; };

struct B : virtual D
{
  int j = i + f();
  int k = A::i + A::f();
};

struct C: B { int pad; };

int main()
{
  C c;
  if (c.j != 84 || c.k != 84)
    __builtin_abort();
}
