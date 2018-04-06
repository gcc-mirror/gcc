// { dg-prune-output "compilation terminated" }

import Bar;
// { dg-error "CRC mismatch" "Foo.nms:" { target *-*-* } 0 }
// { dg-error "failed to import" "Bar.nms:" { target *-*-* } 0 }
// { dg-error "declining" "Bar.nms:" { target *-*-* } 0 }

int bill ();
