// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@lsd.ic.unicamp.br>
// distilled from bug report by Barry M. Caceres <barryc@itravelpartners.com>

// Test whether dtors of vbases are called on delete[].

extern "C" void abort();
extern "C" void exit(int);

struct Foo {
  ~Foo() {
    exit(0);
  }
};

struct Bar : virtual Foo {
};

int main() {
  delete [] new Bar[1];
  abort();
}


