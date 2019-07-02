// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
# 3 __FILE__ 1
export module bob; // { dg-error "must be directly in the main source" }
# 6 "" 2
// { dg-module-cmi !bob }
// { dg-prune-output "not writing module" }
