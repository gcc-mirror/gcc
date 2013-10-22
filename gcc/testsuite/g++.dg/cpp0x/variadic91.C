// { dg-options "-std=c++11" }
template<int N> struct Int2Type { };

template<typename... T>
struct Outer {
  template<typename... U>
  void foo(Int2Type<sizeof...(T)>, Int2Type<sizeof...(U)>);
};


Outer<short, int, long> outer;

void g4() {
  outer.foo<float, double>(Int2Type<3>(), Int2Type<2>());
}

template<typename... T, template<T...> class X> void f1();
