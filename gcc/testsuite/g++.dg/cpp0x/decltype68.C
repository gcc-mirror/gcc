// PR c++/84644
// { dg-do compile { target c++11 } }

template<int a>
struct b {
  decltype(a) __attribute__((break));  // { dg-error "declaration does not declare anything" }
};
