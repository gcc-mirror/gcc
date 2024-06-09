// PR c++/109679
// { dg-module-do link }
// { dg-additional-options "-fmodules-ts" }

import M;
int main() {
  return foo();
}
