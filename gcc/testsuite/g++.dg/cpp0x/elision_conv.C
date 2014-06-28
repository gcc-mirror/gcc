// Core 1579 return by converting move constructor
// PR c++/58051
// { dg-do compile { target c++11 } }

struct A {
  A() = default;
  A(A&&) = default;
};

struct B {
  B(A) { }
};

B f()
{
  A a;
  return a;
}
