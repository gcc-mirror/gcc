// PR c++/114950
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M:b }

module M:b;

struct A {};
extern "C++" struct B {};
void f(int) {}
extern "C++" void f(double) {}
