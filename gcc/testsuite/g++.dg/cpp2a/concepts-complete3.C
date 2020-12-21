// Verify we diagnose an unstable satisfaction result that depends on
// return type deduction of the member function A::foo() below.
//
// { dg-do compile { target c++20 } }

template <class T> concept fooable = requires (T t) { t.foo(); };
// { dg-error "'false' to 'true'" "" { target *-*-* } .-1 }

template <fooable T> int f () { return 0; }
template <class T> char f() { return 0; }

struct A { auto foo(); };
static_assert (sizeof (f<A>()) == 1); // { dg-message "first evaluated to 'false' from here" }
auto A::foo() { }
static_assert (sizeof (f<A>()) > 1); // { dg-error "assert" }
static_assert (sizeof (f<A>()) > 1);
