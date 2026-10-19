// Test both that --compile-std-module works and that P1811 redefinition works
// for all of libstdc++.

// { dg-additional-options "-fmodules --compile-std-module -g -O" }
// { dg-additional-options "-fno-module-include-translate" }
// { dg-additional-options "-fno-module-lazy" }
// { dg-do compile { target c++20 } }
// { dg-module-cmi std }
// { dg-module-cmi std.compat }
// { dg-module-cmi <bits/stdc++.h> }
// { dg-skip-if "Target libm lacks full long double C99 support required by std module" { hppa*-*-hpux* } }

import std;
import std.compat;
#include <bits/stdc++.h>

void f()
{
  std::string s;
}
