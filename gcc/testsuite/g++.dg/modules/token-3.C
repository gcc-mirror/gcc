// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
# 3 __FILE__ 1
export module bob;
# 6 __FILE__ 2
// { dg-module-bmi bob }
