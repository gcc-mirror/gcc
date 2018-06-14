// PR c++/86063
// { dg-do compile { target c++11 } }

template <class... T>
struct S {
  [[foobar(alignof(T))...]] char t; // { dg-warning "attribute directive ignored" }
};
