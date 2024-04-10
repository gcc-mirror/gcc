// PR c++/113580
// { dg-additional-options "-fmodules-ts -Wunused-parameter" }

import A;

int main() {
  fun(42);  // { dg-message "required from here" }
}

// { dg-warning "unused parameter" "" { target *-*-* } 0 }
