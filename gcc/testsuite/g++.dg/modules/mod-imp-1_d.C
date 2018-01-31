// { dg-additional-options "-fdump-lang-module" }
// { dg-final { scan-lang-dump "Starting module Baz" "module" } }
// { dg-final { scan-lang-dump "Nested import Foo" "module" } }
// { dg-final { scan-lang-dump "Starting module Foo" "module" } }

module Baz;
