// Build don't link:
// Special g++ Options: -g

template <class C>
class CenteringTag {
};

struct S {
  template <class B, class C>
  static void f() {
    CenteringTag<C> ctag;
  }
};
