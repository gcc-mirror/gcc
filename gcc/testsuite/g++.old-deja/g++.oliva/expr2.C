// { dg-do run { xfail *-*-* } }

// Copyright (C) 2000 Free Software Foundation

// by Alexandre Oliva <aoliva@cygnus.com>

int i, j;

const int &f(const int& I, const int& J) {
  // this must not be optimized to I because it's an lvalue
  return (I != J) ? I : J;
}

int main () {
  return (&f(i, j) != &j);
}
