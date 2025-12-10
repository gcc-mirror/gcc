// { dg-additional-options "-fmodules --compile-std-module -g -O" }
// { dg-additional-options "-flang-info-include-translate" }
// { dg-do compile { target c++20 } }
// { dg-module-cmi std }
// { dg-module-cmi std.compat }
// { dg-module-cmi <bits/stdc++.h> }

import std;
import std.compat;
#include <vector>		// { dg-message "translated to import" }
import <bits/stdc++.h>;

void f()
{
  std::string s;
}
