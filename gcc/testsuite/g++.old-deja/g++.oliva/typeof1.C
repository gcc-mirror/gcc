// Build don't link:

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>

struct B {
  int i;
};

template <class T> void foo(T b) {
  b.T::i; // ok
  // b.__typeof__(b)::i; // parse error, should this be accepted?
  typedef T t1;
  b.t1::i; // ok
  typedef __typeof__(b) t2;
  b.t2::i; // crash test - XFAIL *-*-*
}

template void foo(B); // not needed for the crash
