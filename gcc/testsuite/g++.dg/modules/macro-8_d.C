// Test that -MM works with a header unit.

// { dg-do preprocess }
// { dg-additional-options "-MM -MF macro-8_d.d -fmodules-ts" }

#include "macro-8_a.H"

#ifndef FOO
#error FOOBAR
#endif

// { dg-final { scan-file macro-8_d.d {macro-8_a.H.gcm} } }
// { dg-final { scan-file-not macro-8_d.i {import *"} } }
