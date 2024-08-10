// PR c++/116276
// { dg-do compile { target c++23 } }

template<class T>
struct Base1 {
  Base1();
  Base1(T);
};

template<class T>
struct Base2 {
  Base2();
  Base2(T*);
};

template<class T = int>
struct Derived : public Base1<T>, Base2<T> {
  using Base1<T>::Base1;
  using Base2<T>::Base2;
};

using ty1 = decltype(Derived{});
using ty1 = Derived<int>;

using ty2 = decltype(Derived{true});
using ty2 = Derived<bool>;

using ty3 = decltype(Derived{(char*)nullptr});
using ty3 = Derived<char>;

template<class T = int>
struct Derived2 : public Base1<T>, Base2<T> {
  using Base1<T>::Base1;
  using Base2<T>::Base2;
  Derived2();
  Derived2(T);
};

using ty4 = decltype(Derived2{});
using ty4 = Derived2<int>;

using ty5 = decltype(Derived2{true});
using ty5 = Derived2<bool>;

using ty6 = decltype(Derived2{(char*)nullptr});
using ty6 = Derived2<char>;
