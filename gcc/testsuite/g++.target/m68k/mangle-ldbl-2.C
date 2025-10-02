// { dg-do compile { target { c++20 && { m68k*-*-linux* } } } }
// { dg-additional-options "-m68020 -fabi-version=20" }
// { dg-final { scan-assembler "_Z3fooILe3fff00008000000000000000EEiv" } }
// { dg-final { scan-assembler "_Z3fooILe400000008000000000000000EEiv" } }

template <long double a>
int foo () { return 0; }

int bar () { return foo <1.0L> () + foo <2.0L> (); }
