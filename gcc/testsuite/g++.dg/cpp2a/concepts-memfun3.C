// { dg-do compile { target c++20 } }

template<int I>
struct A {
  enum E { e = I };
  static void f() requires (e != 0);
};

int main() {
  A<1>::f();
  A<0>::f(); // { dg-error "no match" }
}
