// { dg-additional-options "-fmodules" }
// { dg-timeout-factor 0.05 }

import M;

auto imported_a = make_a ();
struct A : ( // { dg-error "expected" }
// { dg-prune-output "at end of input" }
