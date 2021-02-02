// { dg-additional-options "-fmodules-ts" }

import Foo;

void Frob ()
{
  Bar (); // { dg-error "not declared" }
  Baz (); // { dg-error "not declared" }
}

