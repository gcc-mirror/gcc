// { dg-do assemble  }
// Origin: Chip Salzenberg <chip@perlsupport.com>

class Foo {
  public:
    class Bar;
};

class Foo::Bar {
  public:
    Bar() {}
};
