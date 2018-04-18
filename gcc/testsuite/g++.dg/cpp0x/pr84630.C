// PR c++/84630
// { dg-do compile { target c++11 } }

template <typename...> struct c {
  template <int> __attribute__((noinline([] {}))) int b();  // { dg-error "wrong number of arguments" }
};
c<> a;
