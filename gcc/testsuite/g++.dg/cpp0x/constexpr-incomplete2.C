// A constructor that might or might not be constexpr still makes
// its class literal.
// { dg-options -std=c++0x }

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

constexpr D d {};		// { dg-error "non-constexpr function" }
