// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>
// Special g++ Options:

template <class T>
struct B {
  typedef int I;
};

template <class T, class X = int>
struct S : public B <T> {
  struct I {
  };

  void f(int i = true) {}
};
