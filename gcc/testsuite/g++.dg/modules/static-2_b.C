// PR c++/104433
// { dg-additional-options -fmodules-ts }
// { dg-do link }

import "static-2_a.H";

int main() {
  f();
}
