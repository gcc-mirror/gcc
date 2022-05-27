// PR c++/105652
// { dg-do compile { target c++20 } }
// { dg-additional-options -g }

template<int>
struct I {};

template<class T>
concept C = []<int N>(I<N>) { return true; } (I<0>{});

template<class T>
struct S { };

template<C T>
struct S<T> { constexpr static bool value = true; };

static_assert(S<int>::value);
