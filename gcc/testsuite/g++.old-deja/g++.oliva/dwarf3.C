// { dg-do assemble }
// { dg-xfail-if "" { i386-pc-solaris* } { "*" } { "" } }

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// based on bug report by Andreas Stolcke <stolcke@speech.sri.com>

// Fails with dwarf debugging.

template <class T = void> struct foo {
  int data[1];
};

template <class T = void> struct bar {
  bar(foo<> *);
};

template <class T> bar<T>::bar(foo<> *x) {
  *x;
}

void baz() {
  foo<> *baz;
  bar<> baar(baz);
}
