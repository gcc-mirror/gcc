// PR c++/59120
// { dg-do compile { target c++11 } }

template<typename T> using X = int T::T*;  // { dg-error "expected|two or more" }
