// { dg-do run }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 19 Apr 2003 <nathan@codesourcery.com>

// PR 9881. address-constant-expression not static initialized

struct bar {
  double p;
}; // bar
    
bar v;
static bool error = false;

struct foo {
  static double *a;
  static double *b;
  static double storage;
};

struct baz {
  baz () {
    if (foo::a != &v.p)
      error = true;
    if (foo::b != &foo::storage)
      error = true;
  }
};

baz f; // Get constructor to run before any other non-static initializers

double *foo::a = &(((bar *)(&v))->p);
double *foo::b = &(((bar *)(&foo::storage))->p);
double foo::storage = 0.0;

int main() {
  return error;
}
