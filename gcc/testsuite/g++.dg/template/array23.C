// PR c++/53989

struct Foo {
  int value;
  typedef Foo Foo2;
  static Foo2 const foos[2];
};

template <class T> void g (T);
void bar() {
  g(&Foo::foos);
}
