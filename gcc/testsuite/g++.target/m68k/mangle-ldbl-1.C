// { dg-do compile { target { c++20 && { m68k*-*-linux* } } } }
// { dg-additional-options "-m68020" }
// { dg-final { scan-assembler "_Z3fooILe3fff8000000000000000EEiv" } }
// { dg-final { scan-assembler "_Z3fooILe40008000000000000000EEiv" } }

template <long double a>
int foo () { return 0; }

int bar () { return foo <1.0L> () + foo <2.0L> (); }
