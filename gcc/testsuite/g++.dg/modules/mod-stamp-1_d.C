// { dg-additional-options "-fmodules-ts" }

import Bar;
// { dg-error "CRC mismatch" "" { target *-*-* } 0 }
// { dg-regexp "Foo: error: failed to read compiled module: Bad file data\n" }
// { dg-regexp "Bar: error: failed to read compiled module: Bad import dependency\n" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }

int bill ();
