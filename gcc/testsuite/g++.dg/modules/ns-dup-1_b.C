// { dg-additional-options {-fmodules-ts -Wno-pedantic} }

module;
# 5 __FILE__ 1
namespace std {}
# 7 "" 2
export module bob;
// { dg-module-cmi bob }

import foo;
namespace std {}
