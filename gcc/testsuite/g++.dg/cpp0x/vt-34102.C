// { dg-do compile { target c++11 } }
// PR c++/34102
struct A {};

template<typename> struct B : virtual A {};

template<typename...T> struct C : B<T> {}; // { dg-error "parameter packs|T" }
