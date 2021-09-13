// { dg-do compile { target c++17 } }

struct A { A(); A(int); };
struct B { B(); B(void*); };

template <class... T> struct C: T...
{
  using T::T...;
};

C<A,B> c1(42);
C<A,B> c2(nullptr);
