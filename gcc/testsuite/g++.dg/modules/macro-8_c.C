// Test that -M works with a header unit.

// { dg-do preprocess }
// { dg-additional-options "-M -fmodules-ts" }

import "macro-8_a.H";

#ifndef FOO
#error FOOBAR
#endif

// { dg-final { scan-file macro-8_c.i {macro-8_a.H.gcm} } }
// { dg-final { scan-file-not macro-8_c.i {import *"} } }
