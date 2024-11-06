// { dg-additional-options {-fmodules-ts -fpreprocessed -fdirectives-only} }
// { dg-module-cmi !foo }
module;
#define foo baz
export module foo; // { dg-error "module name 'foo' cannot be an object-like macro" }

class import {};

import x; // { dg-error "post-module-declaration" }
