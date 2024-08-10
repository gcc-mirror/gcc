// PR c++/116276
// { dg-do compile { target c++20 } }

template<class T>
struct Base1 { };

template<class T>
struct Base2 { };

template<class T = int>
struct Derived : public Base1<T>, Base2<T> {
  using Base1<T>::Base1;
  using Base2<T>::Base2;
};

Derived d;

template<class T = int>
struct Derived2 : public Base1<T>, Base2<T> {
  using Base1<T>::Base1::Base1;
  using Base2<T>::Base2::Base2;
  Derived2();
};

Derived2 d2;
