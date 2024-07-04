// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M }

export module M;

struct module {};
struct import {};

void foo() {
  module
    x;  // OK
  import
    y;  // OK
}
