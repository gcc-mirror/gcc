// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
module;
# 4 "gmf" 1
int bar ();
# 6 "" 2
export module thing;
// { dg-module-cmi "thing" }

export int baz ();
