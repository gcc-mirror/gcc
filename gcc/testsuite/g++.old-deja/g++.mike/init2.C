// Build don't link:

struct Foo {
  Foo (int);
};

int bar (Foo);

int x = bar (3);
