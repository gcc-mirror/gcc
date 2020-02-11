// { dg-additional-options {-fmodules-ts -fpreprocessed -fdirectives-only} }
// { dg-module-cmi foo }
__module;
#define foo baz
__export __module foo;

class import {};

import x;
