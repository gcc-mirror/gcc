
module bob;

import bob; // { dg-error "already declared" }

// module linkage
void Baz ()
{
  Foo ();
  Bar ();
}
