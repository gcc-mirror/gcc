// PR c++/104752
// { dg-do compile { target c++23 } }

template<class T>
concept C = true;
auto x = auto(1);     // valid (P0849R8)
auto y = C auto(1);   // { dg-error "cannot be constrained" }
auto z = C auto{1};   // { dg-error "cannot be constrained" }
