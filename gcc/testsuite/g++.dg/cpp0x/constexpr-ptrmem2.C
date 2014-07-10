// PR c++/61661
// { dg-do compile { target c++11 } }

struct Outer {

  void Bar();

  struct Foo {
    void (Outer::*ptr)() ;
  };

  static constexpr Foo foo = { &Outer::Bar };
};
