// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@lsd.ic.unicamp.br>

// Test whether dtors of vbases are called on throw within new[].
// Variant of delete2.C.

extern "C" void abort();
extern "C" void exit(int);

struct Foo {
  static bool first;

  Foo() {
    if (first)
      first = false;
    else
      throw first;
  }

  ~Foo() {
    exit(0);
  }
};

bool Foo::first = true;

struct Bar : virtual Foo {
};

int main() {
  try {
    delete [] new Bar[2];
  } catch (...) {
  }
  abort();
}
