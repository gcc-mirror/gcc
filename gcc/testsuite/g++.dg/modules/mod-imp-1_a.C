// { dg-additional-options "-fdump-front-end" }
// { dg-final { scan-dump "fe" "Writing module 'Foo'" "fe" } }

export module Foo;
// { dg-module-if "Foo" }
