// Verify a conditional explicit-specifier is a SFINAE context.
// { dg-do compile { target c++20 } }

struct A {
  template<class T> explicit(T::value) A(T) = delete;
  A(...);
};

struct B { static bool value; };

A x(0);
A y(B{});
