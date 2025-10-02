// { dg-additional-options "-fmodules-ts" }
module bob;

import bob; // { dg-error "import of 'bob' within its own" }

// module linkage
void Baz ()
{
  Foo ();
  Bar ();
}
