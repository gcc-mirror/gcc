// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
# 1 "other_name"
module ;
export module bob;
// { dg-module-cmi bob }
