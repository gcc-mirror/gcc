// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

module Baz;

// { dg-final { scan-lang-dump "Starting module Baz" "module" } }
// { dg-final { scan-lang-dump "Found import:2 Foo->2" "module" } }
// { dg-final { scan-lang-dump "Starting module Foo" "module" } }
