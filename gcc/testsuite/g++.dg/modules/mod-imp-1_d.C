// { dg-additional-options "-fdump-lang-module" }
// { dg-final { scan-lang-dump "Starting module Baz" "module" } }
// { dg-final { scan-lang-dump "Read import:2 Foo->2" "module" } }
// { dg-final { scan-lang-dump "Starting module Foo" "module" } }

module Baz;
