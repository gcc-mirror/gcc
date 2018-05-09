// { dg-additional-options "-fdump-lang-module" }
// { dg-final { scan-lang-dump "Starting module Foo" "module" } }
// { dg-final { scan-lang-dump "Starting module Baz" "module" } }
// { dg-final { scan-lang-dump "Writing import:2 Foo" "module" } }

export module Baz;
// { dg-module-bmi "Baz" }

import Foo;
