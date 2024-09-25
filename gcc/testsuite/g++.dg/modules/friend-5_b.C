// { dg-additional-options -fmodules-ts }
// From Andrew Sutton

export module bar;
import foo;

class B { // { dg-error "conflicts with import" }
  B() { object.value = 42; }
  A object;
};
