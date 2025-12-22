// PR c++/122958
// { dg-do compile }
template <class>
class D;

template <class R, class S>
class D<R(S)>;

template <class R, class S, class Extra>
class D<R(S)> {}; // { dg-error "template parameters not deducible" }
