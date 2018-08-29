// { dg-options -std=c++17 }

template <class T> struct A {
  A(T&);
  A(const A&);
};

int i;
A a = i;
A a2 = a;

template <class,class> struct same;
template <class T> struct same<T,T> {};
same<decltype(a),A<int>> s1;
same<decltype(a2),A<int>> s2;
