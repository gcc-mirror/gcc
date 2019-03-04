// { dg-additional-options -fmodules-ts }
module;

class X; // { dg-error "global module fragment contents" }

class Y;

export module frob;
// { dg-module-bmi !frob }
// { dg-prune-output "not writing module" }
