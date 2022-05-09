// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
module;
# 4 __FILE__ 1
void Quux ();
# 6 "" 2
export module bob;
// { dg-module-cmi bob }

extern "C++" 
{
export void Bar () {}
export void Quux ();
void Baz () {}
}

// { dg-final { scan-assembler {_Z3Barv:} } }
// { dg-final { scan-assembler {_Z3Bazv:} } }
