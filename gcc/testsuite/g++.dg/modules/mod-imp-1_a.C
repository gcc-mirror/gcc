// { dg-additional-options "-fdump-lang-module" }
// { dg-final { scan-lang-dump "Starting module Foo" "module" } }

export module Foo;
// { dg-module-bmi "Foo" }
