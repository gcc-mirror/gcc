// PR c++/110025
// { dg-do compile { target c++23 } }

template<auto V, class = decltype(auto(V)), class = decltype(auto{V})>
struct A { };

template<auto V>
A<V> f();

int main() {
  f<0>();
}
