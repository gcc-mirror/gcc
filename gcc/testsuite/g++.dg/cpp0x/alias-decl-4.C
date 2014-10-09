// { dg-do compile { target c++11 } }

// [temp.alias]/3:
// The type-id in an alias template declaration shall not refer
// to the alias template being declared. The type produced by an
// alias template specialization shall not directly or indirectly
// make use of that specialization.

template <class T> struct A;
template <class T> using B = typename A<T>::U; // { dg-error "type" }
template <class T> struct A {
    typedef B<T> U;
};
B<short> b;
