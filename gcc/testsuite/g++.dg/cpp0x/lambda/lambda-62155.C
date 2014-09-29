// PR c++/62155
// { dg-do compile { target c++11 } } 

template <typename T> struct S { // { dg-error "cannot convert" }
  T i{[this] {}};
};

S<int> s;                        // { dg-error "cannot convert" }
