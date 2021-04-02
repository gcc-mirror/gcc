// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
module;
# 4 "gmf" 1
import thing.baz;

export int foo (); // { dg-error "after a module interface" }
# 8 "" 2
export module thing.baz; // { dg-error "module already imported" }

import thing.baz;

