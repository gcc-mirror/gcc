// { dg-additional-options "-fmodules-ts" }
module;
export void Foo (); // { dg-error "after a module interface" }

export module bob;
// { dg-module-bmi "!bob" }

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
  import ben; // { dg-error "only occur" }
}
