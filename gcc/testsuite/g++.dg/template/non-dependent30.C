// PR c++/112515
// { dg-do compile { target c++11 } }

enum E : int { };

template<class T>
E f(T t) {
  return E{t.e};
}
