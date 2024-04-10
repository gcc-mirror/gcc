// { dg-module-do link }
// { dg-additional-options "-fmodules-ts" }

import "init-6_a.H";

int main() {
  __from_chars_alnum_to_val();
  get_nonclass_val();
}
