// { dg-do compile { target { c++20 && { x86_64-*-linux* i?86-*-linux* } } } }
// { dg-additional-options "-fabi-version=20" }
// { dg-final { scan-assembler "_Z3fooILe0000000000003fff8000000000000000EEiv" { target { ! ia32 } } } }
// { dg-final { scan-assembler "_Z3fooILe00000000000040008000000000000000EEiv" { target { ! ia32 } } } }
// { dg-final { scan-assembler "_Z3fooILe00003fff8000000000000000EEiv" { target ia32 } } }
// { dg-final { scan-assembler "_Z3fooILe000040008000000000000000EEiv" { target ia32 } } }

template <long double a>
int foo () { return 0; }

int bar () { return foo <1.0L> () + foo <2.0L> (); }
