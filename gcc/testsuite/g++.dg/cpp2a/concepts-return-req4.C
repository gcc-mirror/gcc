// PR c++/104527
// { dg-do compile { target c++20 } }

template<class T, class U>
concept is_same = __is_same(T, U);

template<class T>
struct A {
  template<class...>
    requires requires { { 0 } -> is_same<T>; }
  struct B {};

  template<class...>
    requires requires { { 1 } -> is_same<T>; }
  static void f();
};

A<int>::B<> a1;
A<bool>::B<> a2; // { dg-error "constraint" }

int main() {
  A<int>::f();
  A<bool>::f(); // { dg-error "no match" }
}
