// { dg-additional-options "-fmodules-ts" }

import Bar;
// { dg-error "CRC mismatch" "" { target *-*-* } 0 }
// { dg-error {failed to read module 'Foo.[^']*'} "" { target *-*-* } 0 }
// { dg-error {failed to read module 'Bar.[^']*'} "" { target *-*-* } 0 }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }

int bill ();
