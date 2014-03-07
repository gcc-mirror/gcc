// { dg-do compile { target c++11 } }

extern "C" int printf (const char *, ...);
template< class T >
struct D : T {
  using T::T;
  // declares all constructors from class T
  ~D() { printf ("Destroying wrapper\n"); }
};

struct A {
  A(int);
};

D<A> d(42);
