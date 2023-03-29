// Verify we don't crash when matching constraints containing a
// TEMPLATE_ID_EXPR that names a template from the current instantiation.
// { dg-do compile { target c++20 } }

template<class T> static constexpr bool False = false;

template<class T>
struct A {
  template<int N> static constexpr bool C = sizeof(T) > N;
  friend constexpr void f(A) requires C<1> { }
  friend constexpr void f(A) requires C<1> && False<T> { }
};

template<class T>
struct A<T*> {
  template<int N> static constexpr bool D = sizeof(T) > N;
  friend constexpr void g(A) requires D<1> { }
  friend constexpr void g(A) requires D<1> && False<T> { }
};

int main() {
  f(A<int>{});
  g(A<int*>{});
}
