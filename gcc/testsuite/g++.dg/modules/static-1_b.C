// { dg-additional-options "-fmodules-ts" }

module Foo;

void Frob ()
{
  Bar (); // { dg-error "not declared" }
  // XFAIL until module interface gets its own number
  Baz (); // { dg-error "not declared" "" { xfail *-*-* } }
}

