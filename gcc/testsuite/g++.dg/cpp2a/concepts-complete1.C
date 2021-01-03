// Verify we diagnose an unstable satisfaction result that depends on
// completeness of the type A below.
//
// { dg-do compile { target c++20 } }

template <class T> concept has_mem_type = requires { typename T::type; };
// { dg-message "satisfaction of 'has_mem_type<T>' .with T = A." "" { target *-*-* } .-1 }
// { dg-error "satisfaction value of atomic constraint 'requires.typename T::type;. .with T = A.' changed from 'false' to 'true'" "" { target *-*-* } .-2 }

template <has_mem_type T> int f () { return 0; }
template <class T> char f() { return 0; }

struct A;
static_assert (sizeof (f<A>()) == 1); // { dg-message "first evaluated to 'false' from here" }
struct A { typedef int type; };
static_assert (sizeof (f<A>()) > 1); // { dg-error "assert" }
// { dg-message "required from here" "" { target *-*-* } .-1 }
static_assert (sizeof (f<A>()) > 1);
