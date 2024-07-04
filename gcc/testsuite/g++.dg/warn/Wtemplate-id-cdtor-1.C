// PR c++/107126
// { dg-do compile }
// { dg-options "-Wc++20-compat" }

template<class T>
struct X {
  X<T>(); // { dg-warning "template-id not allowed for constructor" }
  ~X<T>(); // { dg-warning "template-id not allowed for destructor" }
};
