// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
// { dg-module-cmi {mod:impl} }

module;
# 6 "inner" 1
import "part-mac-1_a.H";
# 8 "" 2
module mod:impl;

FOO(k);
