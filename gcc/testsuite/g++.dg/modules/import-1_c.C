// { dg-additional-options "-fmodules-ts -fdump-lang-module" }
// { dg-final { scan-lang-dump "Writing exported import:. Baz" "module" } }
// { dg-final { scan-lang-dump "Writing exported import:. Bar" "module" } }

export module Foo;
// { dg-module-bmi "Foo" }

export import Bar;
export
{
  import Baz;
}
