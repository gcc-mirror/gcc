// { dg-additional-options "-fdump-lang" }
// { dg-final { scan-dump "lang" "Importing 'Foo'" "fe" } }
// { dg-final { scan-dump "lang" "Writing module 'Baz'" "fe" } }
// { dg-final { scan-dump "lang" "Writing import 'Foo'" "fe" } }

export module Baz;
// { dg-module-if "Baz" }

import Foo;
