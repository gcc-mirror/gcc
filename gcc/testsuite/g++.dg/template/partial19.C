// PR c++/67593
// { dg-do compile { target c++11 } }

template<class T>
struct outer {
  template<T...> struct inner;
  template<T... Vs> struct inner<T{}, Vs...> {};
};

outer<int>::inner<0, 0> x1;
outer<int>::inner<1, 0> x2; // { dg-error "incomplete" }
