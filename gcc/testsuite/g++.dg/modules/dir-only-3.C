# 0 "dir-only-3.C"
# 1 "<built-in>"
# 1 "<command-line>"
# 31 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 32 "<command-line>" 2
# 1 "dir-only-3.C"
// { dg-additional-options {-fmodules-ts -fpreprocessed -fdirectives-only} }
// { dg-module-cmi !foo }
module;
#define foo baz
export module foo;
// { dg-error "module name 'foo' cannot be an object-like macro" "" { target *-*-* } 5 }
// { dg-prune-output "not writing module" }

class import {};

import
x;
