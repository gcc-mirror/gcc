// Test for value-initialization via {}
// { dg-options -std=c++0x }
// { dg-do run }

// Empty base so A isn't an aggregate
struct B {};
struct A: B {
  int i;
};

struct C: A {
  C(): A{} {}
};

int f(A a) { return a.i; }

int main()
{
  A a{};
  C c;
  if (a.i != 0
      || c.i != 0
      || A{}.i != 0
      || f({}) != 0)
    return 1;
}
