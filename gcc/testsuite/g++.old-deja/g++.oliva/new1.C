// { dg-do run  }
// { dg-skip-if "requires hosted libstdc++ for stdlib size_t" { ! hostedlib } }

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>

// based on comp.std.c++ post by Alexander Schiemann <aschiem@math.uni-sb.de>

// execution test

#include <new>
#include <stdlib.h>

struct A {
  A() { throw 0; }
  void* operator new(size_t size, double = 0.0) { return ::operator new(size);}
  void operator delete(void* p, double) { exit(0); }
  void operator delete(void* p) { abort(); }
};

int main() { try { new A; } catch(...) {} }
