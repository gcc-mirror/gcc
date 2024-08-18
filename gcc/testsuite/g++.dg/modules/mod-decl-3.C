// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
module;
# 4 "mod-decl-3.C" 1
export void Foo (); // { dg-error "after a module interface" }

# 7 "" 2
export module bob;
// { dg-module-cmi "!bob" }

export
export // { dg-error "occur once" }
void Baz ();

export 
{
  export // { dg-error "occur once" }
    void Bar ();
}

namespace Bink
{
  import  // { dg-error "does not name" }
  ben;
}
