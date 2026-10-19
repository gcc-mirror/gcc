// { dg-additional-options "-fmodules" }
// { dg-module-cmi !P:b }

module P:b;
import :a;

struct A { }; // { dg-error "redefinition" }
