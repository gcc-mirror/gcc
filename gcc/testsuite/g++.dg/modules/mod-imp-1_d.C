// { dg-additional-options "-fdump-lang" }
// { dg-final { scan-dump "lang" "Importing 'Baz'" "fe" } }
// { dg-final { scan-dump "lang" "Begin nested import 'Foo'" "fe" } }
// { dg-final { scan-dump "lang" "Importing 'Foo'" "fe" } }

module Baz;
