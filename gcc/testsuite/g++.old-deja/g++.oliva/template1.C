// { dg-do assemble }

// Copyright (C) 1999, 2003 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// based on bug report by Stefan Wetzel <Stefan_Wetzel@Physik.TU-Muenchen.DE>


template<int P = 0> struct foo {
  static void bar(double (*)[dim]) {} // { dg-error "" } dim not declared
};

void bar() {
  foo<>::bar(0); // { dg-error "" "" } instantiated from here
}
