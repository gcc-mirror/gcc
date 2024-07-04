// PR c++/113708
// { dg-module-do link }
// { dg-additional-options "-fmodules-ts" }

import "init-7_a.H";
int main() { a; }
