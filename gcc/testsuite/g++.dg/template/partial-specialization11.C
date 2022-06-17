// PR c++/86193
// CWG 455 (active)
// { dg-options "" } // clear -pedantic-errors

template<class T> struct value_type;

template<class T, typename value_type<T>::type V>
struct A;

template<class T, int V>
struct A<T*, V> { }; // { dg-warning "not more specialized" }
