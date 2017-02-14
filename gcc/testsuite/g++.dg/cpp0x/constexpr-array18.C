// PR c++/71166
// { dg-do compile { target c++11 } }

struct Foo { int value; };

constexpr Foo MakeFoo() { return Foo{0}; }

struct Bar {
  Foo color = MakeFoo();
};

struct BarContainer {
  Bar array[1];
};

Foo X ()
{
  return MakeFoo ();
}

void Foo() {
  new BarContainer();
}
