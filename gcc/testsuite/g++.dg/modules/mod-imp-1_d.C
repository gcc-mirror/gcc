// { dg-additional-options "-fdump-lang-module" }
// { dg-final { scan-lang-dump "Importing 'Baz'" "module" } }
// { dg-final { scan-lang-dump "Begin nested import 'Foo'" "module" } }
// { dg-final { scan-lang-dump "Importing 'Foo'" "module" } }

module Baz;
