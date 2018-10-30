// { dg-additional-options "-fmodules-ts" }

module frob;

namespace 
{
  // separate module idents
  void *nope; // { dg-bogus "conflicting" "sees interface"  }
}

void *q ()
{
  f ();
  g ();
  return nope; // { dg-bogus "was not declared" "doesn't see interface" }
}
