// PR c++/79653
// { dg-do compile { target c++11 } }

template <typename... T>
struct A { alignas(int...) char c; }; // { dg-error "no argument packs|expected" }
A<int, double> a;
