// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

module Baz;

// { dg-final { scan-lang-dump "Starting module Baz" "module" } }
// { dg-final { scan-lang-dump "Found import:1 Foo->1" "module" } }
// { dg-final { scan-lang-dump "Starting module Foo" "module" } }
