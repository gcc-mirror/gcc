// { dg-additional-options "-fdump-lang" }
// { dg-final { scan-dump "lang" "Writing module 'Foo'" "lang" } }

export module Foo;
// { dg-module-if "Foo" }
