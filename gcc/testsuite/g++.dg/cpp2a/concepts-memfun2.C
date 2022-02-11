// PR c++/103783
// { dg-do compile { target c++20 } }

template<bool B>
struct A {
  template<class...> void f1() = delete;
  template<class...> static void f1() requires B;

  template<class...> void f2() requires B;
  template<class...> static void f2() = delete;

  void g1() = delete;
  static void g1() requires B;

  void g2() requires B;
  static void g2() = delete;
};

int main() {
  A<true> a;
  a.f1(); // OK
  a.f2(); // OK
  a.g1(); // OK, previously rejected as ambiguous
  a.g2(); // OK, previously rejected as ambiguous
}
