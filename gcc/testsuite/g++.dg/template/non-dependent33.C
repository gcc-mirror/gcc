// PR c++/115139
// { dg-do compile { target c++11 } }

template<class... Ts>
class A {
  enum E {
    e1 = 1,
    e2 = 2,
    e3 = e1 | e2,
  };
};

template class A<int>;
