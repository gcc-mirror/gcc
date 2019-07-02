// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

export module Foo;
// { dg-module-cmi "Foo" }

// { dg-final { scan-lang-dump "Starting module Foo" "module" } }
