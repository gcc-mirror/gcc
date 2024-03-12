// PR c++/109752
// { dg-do compile { target c++20 } }

template<class T>
concept C = requires { sizeof(T); } && T::value; // { dg-error "changed from" }

struct A;

static_assert(!C<A>);

struct A { static constexpr bool value = false; };

static_assert(C<A>); // { dg-error "assert" }
