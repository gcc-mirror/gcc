// { dg-additional-options -fmodules-ts }
// { dg-do link }

import "tpl-spec-8_a.H";

int main() {
  A<int>::f();
}
