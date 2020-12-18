// Verify we diagnose an unstable satisfaction result that depends on
// completeness of the type A below.
//
// Like in the previous test, here satisfaction also initially fails,
// but this time due to failed substitution into the atom's parameter mapping.
//
// { dg-do compile { target c++20 } }

template <class T> concept valid_type = requires { typename T; };
// { dg-message "satisfaction of 'valid_type<typename T::type>' .with T = A." "" { target *-*-* } .-1 }
// { dg-error "satisfaction value of atomic constraint 'requires.T;. .with T = typename T::type.' changed from 'false' to 'true'" "" { target *-*-* } .-2 }

template <class T> concept has_mem_type = valid_type<typename T::type>;
// { dg-message "satisfaction of 'has_mem_type<T>' .with T = A." "" { target *-*-* } .-1 }

template <has_mem_type T> int f () { return 0; }
template <class T> char f() { return 0; }

struct A;
static_assert (sizeof (f<A>()) == 1); // { dg-message "first evaluated to 'false' from here" }
struct A { typedef int type; };
static_assert (sizeof (f<A>()) > 1); // { dg-error "assert" }
static_assert (sizeof (f<A>()) > 1);
