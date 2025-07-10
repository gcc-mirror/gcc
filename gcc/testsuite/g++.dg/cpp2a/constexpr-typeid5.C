// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++17 } }

#include <typeinfo>

template <class T>
constexpr bool foo ()
{
   bool r = false;
   const std::type_info &s = typeid( (r = true), *(T *) 0);	// { dg-error "call to non-'constexpr' function 'void __cxa_bad_typeid\\\(\\\)'" "" { target c++23_down } }
   return r;							// { dg-message "declared here" "" { target c++23_down } .-1 }
}

struct A {};
struct B { virtual ~B () {} };

static_assert (!foo <int> ());
static_assert (!foo <A> ());
constexpr bool a = foo <B> ();	// { dg-message "in 'constexpr' expansion of" "" { target c++23_down } }
// { dg-error "uncaught exception" "" { target c++26 } .-1 }
