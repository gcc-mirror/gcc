// { dg-options "-fmodules-atom" }

// These are different modules (by default).
import "frob";
import <frob>;

// { dg-error "failed to read module 'frob.u.nms'" "" { target *-*-* } 0 }
// { dg-error "failed to read module 'frob.s.nms'" "" { target *-*-* } 0 }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }
