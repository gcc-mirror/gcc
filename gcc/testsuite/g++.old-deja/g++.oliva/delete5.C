// { dg-do run  }
// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@lsd.ic.unicamp.br>

// Test whether dtors of vbases are called from dtor of auto array.
// Variant of delete2.C, delete3.C and delete4.C.

extern "C" void abort();
extern "C" void exit(int);

struct Foo {
  ~Foo() {
    exit(0);
  }
};

struct Bar : virtual Foo {
};

void foo() {
  Bar i[1];
}

int main() {
  foo();
  abort();
}


