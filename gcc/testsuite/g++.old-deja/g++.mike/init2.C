// { dg-do assemble  }

struct Foo {
  Foo (int);
};

int bar (Foo);

int x = bar (3);
