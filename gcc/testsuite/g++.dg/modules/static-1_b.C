// { dg-additional-options "-fmodules-ts" }
module Foo;

void Frob ()
{
  Bar (); // { dg-error "not declared" }
  Baz (); // { dg-error "not declared" }
}

