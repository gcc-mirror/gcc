// PR c++/112580
// { dg-additional-options "-fmodules-ts" }

import "anon-3_a.H";

int main() {
  vformat()._M_format_arg();
}
