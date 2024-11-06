// { dg-additional-options {-fmodules-ts -fpreprocessed -fdirectives-only} }
// { dg-module-cmi !baz }
module;
#define foo baz
export module baz;

class import {};

import x; // { dg-error "post-module-declaration" }
