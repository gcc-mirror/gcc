// PR c++/122609
// PR c++/101140
// { dg-additional-options "-fmodules" }

import M;

int main() {
  use_gmf(G{});  // { dg-bogus "" "PR122609" { xfail *-*-* } }
  // { dg-prune-output "cannot decompose" }

  // operator new is not visible from point of definition,
  // and is also not visible from this TU (point of instantiation),
  // so the call below should fail.
  use_future_decl(F{});  // { dg-error "" "PR122609" { xfail *-*-* } }
}
