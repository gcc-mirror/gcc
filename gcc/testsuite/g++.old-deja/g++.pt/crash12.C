// { dg-do assemble  }
// { dg-options "-g" }

template <class C>
class CenteringTag {
};

struct S {
  template <class B, class C>
  static void f() {
    CenteringTag<C> ctag;
  }
};
