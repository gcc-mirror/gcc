// PR c++/100634
// { dg-do compile { target c++20 } }
// { dg-options "" }

// We could support _Complex template arguments, but better I think to make
// people use a standard type instead.
template<_Complex int> struct ComplexInt {}; // { dg-error "not a valid type" }
using CI = ComplexInt<1 + 3i>;
