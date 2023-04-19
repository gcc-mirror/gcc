// PR c++/109556
// { dg-do compile { target c++20 } }

template<class T, auto N>
concept C = (N != 0);

template<auto N, auto M>
struct A { };

template<auto N, C<N> auto M>
void f(A<N, M>);

int main() {
  f(A<1, 42>{});
  f(A<2, 42>{});
  f(A<1, 43>{});
  f(A<2, 43>{});
}
