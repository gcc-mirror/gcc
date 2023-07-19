// PR c++/80956
// { dg-do compile { target c++11 } }

namespace std {
template <class> class initializer_list;
}

template <typename T> struct B { B (std::initializer_list<T>); };
struct C { virtual int foo (); };
struct D : C {} d { B<C> { D {} } };  // { dg-error "no matching" }
