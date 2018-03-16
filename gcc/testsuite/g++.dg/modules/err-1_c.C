// { dg-additional-options "-fmodules-ts" }
module Foo;
// { dg-message "-: Frob\\(int, int, long int\\)" "Foo.nms" { target *-*-* } 0 }
// { dg-message "-: Frob@Foo\\(int, long int, int\\)" "Foo.nms" { target *-*-* } 0 }

void One ()
{
  Frob (0, 0, 0L);
  Frob (0, 0L, 0);
  Frob (0L, 0, 0); // { dg-error "ambiguous" }
}

import Bar;
// { dg-message "-: Frob\\(long int, int, int\\)" "Foo.nms" { target *-*-* } 0 }

void Two ()
{
  Frob (0L, 0, 0);
  Frob (0, 0, 0); // { dg-error "ambiguous" }
}
