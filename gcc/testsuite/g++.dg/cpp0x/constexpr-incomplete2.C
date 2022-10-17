// A constructor that might or might not be constexpr still makes
// its class literal.
// { dg-do compile { target c++11 } }

template <class T>
struct B
{
  constexpr B(T) { }
  constexpr B() {}
};

struct A
{
  B<A> b;
};

constexpr A a {};

template <class T>
struct C
{
  constexpr C(T) { }
  C() {}
};

struct D
{
  C<D> c;
};

constexpr D d {};		// { dg-error "non-.constexpr. function" "" { target { ! implicit_constexpr } } }
