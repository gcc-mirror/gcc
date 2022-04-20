// PR c++/105110
// { dg-do compile { target c++20 } }

template<class> struct A { };

template<auto> struct B { };

template<class T, A<T> V> void f(B<V>);

int main() {
  constexpr A<int> a;
  f(B<a>{});
}
