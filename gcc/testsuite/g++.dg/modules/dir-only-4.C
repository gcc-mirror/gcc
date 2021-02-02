// { dg-additional-options {-fmodules-ts -fpreprocessed -fdirectives-only} }
// { dg-module-cmi !foo }
module;
#define foo baz
export module foo;

class import {};

import x; // { dg-error "post-module-declaration" }
 // { dg-prune-output "not writing module" }
