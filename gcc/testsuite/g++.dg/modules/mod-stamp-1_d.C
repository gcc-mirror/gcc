// { dg-additional-options "-fmodules-ts" }

import Bar;
// { dg-error "CRC mismatch" "" { target *-*-* } 0 }
// { dg-regexp "Foo: error: failed reading from 'Foo.gcm': Bad file data\n" }
// { dg-regexp "Bar: error: failed reading from 'Bar.gcm': Bad import dependency\n" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }

int bill ();
