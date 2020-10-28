// PR c++/97511
// { dg-do compile { target c++11 } }

template <class Z> using Z = Z; // { dg-error "shadow|declaration" }
