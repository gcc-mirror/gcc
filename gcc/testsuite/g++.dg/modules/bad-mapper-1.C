//  { dg-additional-options "-fmodules-ts -fmodule-mapper=|this-will-not-work" }
import unique1.bob;
// { dg-error "-:failed (exec|CreateProcess|posix_spawn).*mapper.* .*this-will-not-work" "" { target { ! { hppa*-*-hpux* } } } 0 }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "failed to read" }
// { dg-prune-output "compilation terminated" }
// { dg-error "-:failed mapper handshake communication" "" { target { hppa*-*-hpux* } } 0 }
// { dg-prune-output "trying to exec .this-will-not-work."  }
// { dg-prune-output "unknown Compiled Module Interface"  }
