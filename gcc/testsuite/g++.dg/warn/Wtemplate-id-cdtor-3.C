// PR c++/107126
// { dg-do compile }
// { dg-options "-Wc++20-compat -Wno-template-id-cdtor" }

template<class T>
struct X {
  X<T>(); // { dg-bogus "template-id not allowed for constructor" }
  ~X<T>(); // { dg-bogus "template-id not allowed for destructor" }
};
