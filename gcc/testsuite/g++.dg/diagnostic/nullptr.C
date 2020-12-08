// PR c++/97517
// { dg-do compile { target c++20 } }
// Test that we print "decltype(nullptr)" correctly.

template<typename T> struct Trait { static constexpr bool value = false; };
template<typename T> concept Concept = Trait<T>::value; // { dg-message {\[with T = std::nullptr_t\]} }
static_assert( Concept<decltype(nullptr)> ); // { dg-error "static assertion failed" }
// { dg-message "constraints not satisfied" "" { target *-*-* } .-1 }
