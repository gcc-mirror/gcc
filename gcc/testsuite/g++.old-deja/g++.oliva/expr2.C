// Copyright (C) 2000 Free Software Foundation

// by Alexandre Oliva <aoliva@cygnus.com>

// execution test - XFAIL *-*-*

extern "C" void abort (void);
extern "C" void exit (int);

int i, j;

const int &f(const int& I, const int& J) {
  // this must not be optimized to I because it's an lvalue
  return (I != J) ? I : J;
}

int main () {
  if (&f(i, j) != &j)
    abort ();
  exit (0);
}
