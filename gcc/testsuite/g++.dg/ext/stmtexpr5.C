// PR c++/21440
// { dg-options "" }

struct Foo {
  ~Foo();
  int i;
};

void bar() {
  Foo foo = ({
    Foo bletch;
    bletch.i = 0;
    bletch;
  });
}
