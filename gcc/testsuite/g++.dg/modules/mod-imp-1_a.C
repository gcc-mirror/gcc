// { dg-additional-options "-fdump-lang-module" }
// { dg-final { scan-lang-dump "Writing module Foo" "module" } }

export module Foo;
// { dg-module-bmi "Foo" }
