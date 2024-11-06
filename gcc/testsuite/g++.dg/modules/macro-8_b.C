// Test that -dM works with a header unit.

// { dg-do preprocess }
// { dg-additional-options "-dM -fmodules-ts" }

import "macro-8_a.H";

#ifndef FOO
#error FOOBAR
#endif

// { dg-final { scan-file macro-8_b.i {#define FOO foo} } }
// { dg-final { scan-file-not macro-8_b.i {import *"} } }
