// { dg-additional-options "-fdump-lang" }
// { dg-final { scan-dump "lang" "Writing export import 'Baz'" "lang" } }
// { dg-final { scan-dump "lang" "Writing export import 'Bar'" "lang" } }

export module Foo;
// { dg-module-if "Foo" }

export import Bar;
export
{
  import Baz;
}
