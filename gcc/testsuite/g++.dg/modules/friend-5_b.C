// { dg-additional-options -fmodules-ts }
// From Andrew Sutton

export module bar;
import foo;

class B { // { dg-error "in a different module" }
  B() { object.value = 42; }
  A object;
};
// { dg-prune-output "not writing module" }
