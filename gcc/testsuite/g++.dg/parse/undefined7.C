// PR c++/9228
// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>
// { dg-do compile }

template <typename T> typename A<T>::B foo() {} // { dg-error "" }
