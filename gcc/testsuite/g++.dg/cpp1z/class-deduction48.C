// PR c++/82468
// { dg-do compile { target c++17 } }

template <template <class> class TT>
TT(double) -> TT<int>;  // { dg-error "template template" }
