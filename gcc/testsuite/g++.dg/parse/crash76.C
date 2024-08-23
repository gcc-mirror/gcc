// PR c++/113746
// { dg-do compile }

template<typename_T> struct S { // { dg-error "not been declared" }
  enum { e0 = 0, e00 = e0 };
};
