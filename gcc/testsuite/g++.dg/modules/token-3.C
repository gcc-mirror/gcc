// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
# 3 __FILE__ 1
export module bob; // { dg-error "in included file" }
# 6 "" 2
// { dg-module-cmi !bob }
