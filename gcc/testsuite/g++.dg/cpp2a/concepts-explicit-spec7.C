// PR c++/107522
// { dg-do compile { target c++20 } }

template<class T>
struct A {
  template<int N>
  static void f() requires (N == 42);

  template<class U>
  struct B {
    template<int N>
    static void g() requires (T(N) == 42);
  };
};

template<>
template<int N>
void A<int>::f() requires (N == 42) { }

template<>
template<>
template<int N>
void A<int>::B<int>::g() requires (int(N) == 42) { }

int main() {
  A<int>::f<42>();
  A<int>::f<43>(); // { dg-error "no match" }
  A<int>::B<int>::g<42>();
  A<int>::B<int>::g<43>(); // { dg-error "no match" }
}
