// { dg-prune-output "compilation terminated" }

import Bar;
// { dg-error "CRC mismatch" "" { target *-*-* } 0 }
// { dg-error "failed to read BMI 'Foo.nms'" "" { target *-*-* } 0 }
// { dg-error "failed to read BMI 'Bar.nms'" "" { target *-*-* } 0 }
// { dg-prune-output "fatal error" }


int bill ();
