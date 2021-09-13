// { dg-additional-options -fmodule-header }
// { dg-module-cmi !{} }
import "pr99174.H"; // { dg-error "cannot import" }

// { dg-prune-output {not writing module} }
