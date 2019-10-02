// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

export module Baz;
// { dg-module-cmi "Baz" }

import Foo;

// { dg-final { scan-lang-dump "Starting module Foo" "module" } }
// { dg-final { scan-lang-dump "Starting module Baz" "module" } }
// { dg-final { scan-lang-dump "Writing import:1->1 Foo" "module" } }
