// { dg-do run  }
// Test of class-scope using-declaration for functions.

#define assert(COND) if (!(COND)) return 1

struct A {
  int f(int) { return 1; }
  int f(char) { return 2; }
};

struct B {
  int f(double) { return 3; }
};

struct C : public A, public B {
  using A::f;
  using B::f;
  int f(char) { return 4; }
  int f(C) { return 5; }
};

int main ()
{
  C c;

  assert (c.f(1) == 1);
  assert (c.f('a') == 4);
  assert (c.f(2.0) == 3);
  assert (c.f(c) == 5);
}
