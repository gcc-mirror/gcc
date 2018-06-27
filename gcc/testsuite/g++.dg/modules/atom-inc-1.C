// { dg-options "-fmodules-atom" }

// These are the same module.
import "frob";
import <frob>;

// { dg-error "failed to read module 'frob.rmf'" "" { target *-*-* } 0 }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }
