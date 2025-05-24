// PR c++/120412
// { dg-additional-options "-fmodules -std=c++20" }

import m;

int main() {
  // { dg-error "instantiation exposes TU-local entity '(fun1|Dodgy)'" "" { target *-*-* } 0 }
  fun2(123);  // { dg-message "required from here" }
}
