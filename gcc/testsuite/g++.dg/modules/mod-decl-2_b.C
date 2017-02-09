
module bob;

import bob; // { dg-error "already declared" }

// module linkage
void Baz ()
{
  // FIXME: Implement modules
  Foo (); // { dg-bogus "not declared" "modules unimplemented" { xfail *-*-* } }
  Bar (); // { dg-bogus "not declared" "modules unimplemented" { xfail *-*-* } }
}
