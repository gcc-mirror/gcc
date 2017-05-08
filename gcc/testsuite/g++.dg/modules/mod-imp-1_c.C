// { dg-additional-options "-fdump-front-end" }
// { dg-final { scan-dump "fe" "Importing 'Foo'" "fe" } }
// { dg-final { scan-dump "fe" "Writing module 'Baz'" "fe" } }
// { dg-final { scan-dump "fe" "Writing import 'Foo'" "fe" } }

export module Baz;
// { dg-module-if "Baz" }

import Foo;
