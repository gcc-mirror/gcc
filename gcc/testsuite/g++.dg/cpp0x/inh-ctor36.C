// { dg-do compile { target c++11 } }

struct A { A(); A(int); };

template <class... T> struct C: T...
{
  using A::A;
};

C<A> c1(42);
