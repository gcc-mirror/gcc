// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
# 3 __FILE__ 1
module ; // { dg-warning "" }
export module bob; // { dg-warning "" }
# 6 __FILE__ 2
// { dg-module-bmi bob }
