//  { dg-additional-options "-fmodules-ts -fmodule-mapper=|this-will-not-work" }
import unique1.bob;
// { dg-error "-:failed exec.*mapper.* .*this-will-not-work" "" { target *-*-* } 0 }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "failed to read" }
// { dg-prune-output "compilation terminated" }
