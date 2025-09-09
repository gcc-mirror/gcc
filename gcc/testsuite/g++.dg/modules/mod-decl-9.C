// PR c++/121808
// { dg-additional-options "-fmodules" }
// { dg-module-cmi !foo }

void test();

export module foo;  // { dg-error "module-declaration" }

import foo;
// { dg-error "failed to read compiled module" "" { target *-*-* } 0 }

// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }
