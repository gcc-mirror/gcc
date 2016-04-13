// Test for sibcall optimization with empty struct.
// { dg-options "-O2" }
// { dg-final { scan-assembler "jmp" { target i?86-*-* x86_64-*-* } } }

struct A { };
void f(struct A);
void g(struct A a) { f(a); }
