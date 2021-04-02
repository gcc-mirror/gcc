// PR c++/96623
// { dg-do compile { target c++11 } }

constexpr int x = 0;
struct A {
  int a1;
  void foo (int p) {
    int foovar;
    struct B {
      int b1;
      void bar1 () noexcept(x);
      void bar2 () noexcept(noexcept(this->b1));
      void bar3 () noexcept(noexcept(this->b2));
      void bar4 () noexcept(noexcept(a1));
      void bar5 () noexcept(noexcept(a2));
      void bar6 () noexcept(noexcept(b1));
      void bar7 () noexcept(noexcept(b2));
      void bar8 () noexcept(noexcept(foovar));
      void bar9 () noexcept(noexcept(p));
      int b2;
    };
  }
  int a2;
};
