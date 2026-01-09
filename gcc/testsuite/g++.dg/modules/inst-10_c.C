// PR c++/122609
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodules" }

import U;

int main() {
  // std::tuple_size and std::tuple_element aren't visible from
  // use_without_import, and also aren't visible from here,
  // so despite being visible on the instantiation path this is an error.

  call_use_without_import(S{});  // { dg-message "required from here" }
  // { dg-error {cannot decompose class type} "" { target *-*-* } 0 }
}
