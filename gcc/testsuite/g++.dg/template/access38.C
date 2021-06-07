// PR c++/100918

struct Outer {
  template<class T>
  struct Inner { ~Inner(); };
};

template<>
Outer::Inner<int>::~Inner<int>() { } // { dg-error "template-id" "" { target c++20 } }

template<class T>
Outer::Inner<T>::~Inner<T>() { } // { dg-error "template-id" "" { target c++20 } }

Outer::Inner<int> x;
Outer::Inner<char> y;
