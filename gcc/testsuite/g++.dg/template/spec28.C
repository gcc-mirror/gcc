// PR c++/25854
// Bad diagnostic
// { dg-do compile }

template<typename> struct A {};  // { dg-error "provided" }
template<> struct A<> {};        // { dg-error "wrong number" }
