// { dg-additional-options "-fdump-front-end" }
// { dg-final { scan-dump "fe" "Importing 'Baz'" "fe" } }
// { dg-final { scan-dump "fe" "Begin nested import 'Foo'" "fe" } }
// { dg-final { scan-dump "fe" "Importing 'Foo'" "fe" } }

module Baz;
