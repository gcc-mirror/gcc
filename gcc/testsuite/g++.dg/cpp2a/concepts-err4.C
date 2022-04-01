// PR c++/105067
// { dg-do compile { target c++20 } }

template<class>
template<class>
concept C = true; // { dg-error "parameter list" }
