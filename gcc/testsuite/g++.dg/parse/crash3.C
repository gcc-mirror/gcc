template <class T> struct L { struct I {}; };
template <class T> void L<T>::I::foo() {} // { dg-error "" }
