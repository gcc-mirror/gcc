// PR c++/32870

struct Foo {
  struct Bar;
};

namespace N {
  struct Foo::Bar { }; // { dg-error "in namespace 'N'" }
}

struct Baz {
  struct Foo::Bar { }; // { dg-error "in 'struct Baz'" }
};
