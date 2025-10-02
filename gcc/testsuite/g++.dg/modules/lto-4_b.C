// PR c++/121865
// { dg-module-do link }
// { dg-require-effective-target lto }
// { dg-additional-options "-fmodules -flto" }

import M;
template struct S<int>;
int main() {}
