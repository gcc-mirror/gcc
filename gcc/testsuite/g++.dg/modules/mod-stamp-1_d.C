// { dg-prune-output "compilation terminated" }

import Bar;
// { dg-error "CRC mismatch" "" { target *-*-* } 0 }
// { dg-error "failed to read module 'Foo.nms'" "" { target *-*-* } 0 }
// { dg-error "failed to read module 'Bar.nms'" "" { target *-*-* } 0 }
// { dg-prune-output "fatal error" }


int bill ();
