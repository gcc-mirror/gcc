// { dg-additional-options "-fdump-lang-module" }
// { dg-final { scan-lang-dump "Writing export import Baz" "module" } }
// { dg-final { scan-lang-dump "Writing export import Bar" "module" } }

export module Foo;
// { dg-module-bmi "Foo" }

export import Bar;
export
{
  import Baz;
}
