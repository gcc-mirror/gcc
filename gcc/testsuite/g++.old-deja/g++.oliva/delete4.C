// { dg-do run  }
// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@lsd.ic.unicamp.br>

// Test whether dtors of vbases are called from dtor of aggregate of array.
// Variant of delete2.C and delete3.C.

extern "C" void abort();
extern "C" void exit(int);

struct Foo {
  ~Foo() {
    exit(0);
  }
};

struct Bar : virtual Foo {
};

struct Baz {
  Bar i[1];
};

int main() {
  Baz();
  abort();
}


