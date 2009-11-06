// PR c++/32112

template<typename> struct A;

template<typename T> void foo (A<&T::template i>); // { dg-error "T:: ?template i|mismatch|& T::i" }
