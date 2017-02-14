// { dg-additional-options "-fdump-front-end" }
// { dg-final { scan-dump "fe" "Writing module 'Foo'" "fe" } }

module Foo [[interface]];
// { dg-module-if "Foo" }
