// Build don't link:

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// based on bug report by Stefan Wetzel <Stefan_Wetzel@Physik.TU-Muenchen.DE>

// crash test - XFAIL *-*-*

template<int P = 0> struct foo {
  static void bar(double (*)[dim]) {} // ERROR - dim not declared
};

void bar() {
  foo<>::bar(0); // ERROR - instantiated from here
}
