// { dg-do compile { target c++11 } }
// { dg-options "" }

template<class T> struct Foo { Foo(T = nullptr) {} };

static_assert (!__is_constructible(Foo<int>));
