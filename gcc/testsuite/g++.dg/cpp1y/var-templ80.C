// PR c++/97340
// { dg-do compile { target c++14 } }

template<class>
struct A {
  template<class>
  static constexpr const int& var = 0;
};

template<class T>
struct B {
  static constexpr int x1 = A<T>::template var<int>;
  static constexpr int y1 = A<T>{}.template var<int>;

  static constexpr int x2 = A<int>::template var<T>;
  static constexpr int y2 = A<int>{}.template var<T>;

  static constexpr int x3 = A<int>::template var<int>;
  static constexpr int y3 = A<int>{}.template var<int>;
};

template struct B<int>;
