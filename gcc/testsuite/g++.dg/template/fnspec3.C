// PR c++/63378
// { dg-do compile { target c++11 } }

template<class T1, class S1>
struct B { };

template<class T1>
struct A {
private:
  template<class T2, class S2>
  static B<T2, S2> g();

public:
  template<class S2>
  auto f() -> decltype(g<T1, S2>());
};

template<>
template<>
auto A<int>::f<float>() -> B<int, float>;
