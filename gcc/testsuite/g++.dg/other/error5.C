// { dg-do compile }
// Origin: Wolfgang Bangerth <bangerth@ices.utexas.edu>
// PR c++/11106: Error message gives partially mangled operator name

template <typename T> struct S {
    struct I {};
};

template <typename T> struct S2 : S<T> {
    using S<T>::operator typename S<T>::I*; // { dg-error "operator S\\<int\\>" "" }
};

template struct S2<int>;  // { dg-message "instantiated" "" }
