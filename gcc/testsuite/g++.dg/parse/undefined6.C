// PR c++/8596
// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>
// { dg-do compile }

template <int i> struct A {};
template <int i> struct B : A<x>{}; // { dg-error "" }
