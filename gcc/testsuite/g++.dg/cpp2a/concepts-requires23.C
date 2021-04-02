// { dg-do compile { target c++20 } }

// Verify f<A>'s associated constraints evaluate to false due
// to return type deduction failure for A::foo().

template <class T> concept fooable = requires { T::foo(0); };
template <fooable T> int f ();
struct A { static auto *foo(auto); };
int a = f<A>(); // { dg-error "no match" }
