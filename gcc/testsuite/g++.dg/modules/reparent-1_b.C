// { dg-additional-options {-fmodules-ts -Wno-pedantic} }
// { dg-module-cmi bar }


module;
# 7 "gmf1" 1
# 8 "gmf2" 1
# 9 "gmf3" 1
import foo;
# 11 "" 2
# 12 "" 2
# 13 "" 2
export module bar;
export import foo;
