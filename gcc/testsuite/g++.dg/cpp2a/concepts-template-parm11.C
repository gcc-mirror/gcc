// PR c++/100374
// { dg-do compile { target c++20 } }

template<class T, class U>
concept C = requires { typename T; };

template<class T>
struct A {
  template<C<typename T::value_type> U>
  void f();

  template<C<typename T::value_type> U>
  struct B;
};

int main() {
  A<int> a;
  a.f<void>();
  using type = A<int>::B<void>;
}
