namespace NS_1 {
  struct A {};
  struct foo {};
}

namespace NS_2 {
  template <typename T> void foo(T);
  
  template <typename T>
  void bar() {
    NS_1::A a;
    NS_2::foo(a);
  }

  template void bar<int>();
}
