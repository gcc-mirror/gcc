// { dg-additional-options "-fdump-lang" }
// { dg-final { scan-dump "lang" "Writing export import 'Baz'" "fe" } }
// { dg-final { scan-dump "lang" "Writing export import 'Bar'" "fe" } }

export module Foo;
// { dg-module-if "Foo" }

export import Bar;
export
{
  import Baz;
}
