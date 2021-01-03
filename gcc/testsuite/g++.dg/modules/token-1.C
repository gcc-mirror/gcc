// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
# 3 __FILE__ 1
module ; // { dg-error "cannot be in included file" }
export module bob; // { dg-error "cannot be in included file" }
# 6 "" 2
// { dg-module-cmi !bob }
// { dg-prune-output "not writing module" }
