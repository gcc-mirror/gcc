// PR c++/25854
// Bad diagnostic
// { dg-do compile }

template<typename> struct A {};  // { dg-message "provided for" }
template<> struct A<> {};        // { dg-error "wrong number" }
