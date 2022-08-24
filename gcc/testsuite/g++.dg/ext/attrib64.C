// PR c++/104667
// { dg-do compile }

template<typename> struct A {
  enum E { // { dg-warning "only applies to function types" }
    e __attribute__ ((access(read_only))),
    f __attribute__ ((deprecated))
  };
};

A<int> a;
