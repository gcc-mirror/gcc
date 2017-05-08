// { dg-additional-options "-fdump-front-end" }
// { dg-final { scan-dump "fe" "Writing export import 'Baz'" "fe" } }
// { dg-final { scan-dump "fe" "Writing export import 'Bar'" "fe" } }

export module Foo;
// { dg-module-if "Foo" }

export import Bar;
export
{
  import Baz;
}
