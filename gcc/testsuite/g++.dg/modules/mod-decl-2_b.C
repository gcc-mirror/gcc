// { dg-additional-options "-fmodules-ts" }
module bob;

import bob; // { dg-error "cannot import module.* in its own purview" }

// module linkage
void Baz ()
{
  Foo ();
  Bar ();
}
