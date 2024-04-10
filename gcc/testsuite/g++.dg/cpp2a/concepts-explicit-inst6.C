// PR c++/104634
// { dg-do compile { target c++20 } }

// { dg-final { scan-assembler "_ZN1AIiE2f1Ev" } }
// { dg-final { scan-assembler "_ZN1AIdE2f2Ev" } }
// { dg-final { scan-assembler "_ZN1AIPiE2f3Ev" } }
// { dg-final { scan-assembler "_ZN1AIPdE2f4Ev" } }

template<class T>
struct A { };

template<class T> requires __is_same(T, int)
struct A<T> {
  void f1() { }
};

template<class T> requires __is_same(T, double)
struct A<T> {
  void f2() { }
};

template<class T> requires __is_same(T, int)
struct A<T*> {
  void f3() { }
};

template<class T> requires __is_same(T, double)
struct A<T*> {
  void f4() { }
};

template struct A<int>;
template struct A<double>;
template struct A<int*>;
template struct A<double*>;
