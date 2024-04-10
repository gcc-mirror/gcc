// PR c++/107126
// { dg-do compile }
// { dg-options "-Wtemplate-id-cdtor -Wno-c++20-compat" }

template<class T>
struct X {
  X<T>(); // { dg-warning "template-id not allowed for constructor" }
  ~X<T>(); // { dg-warning "template-id not allowed for destructor" }
};
