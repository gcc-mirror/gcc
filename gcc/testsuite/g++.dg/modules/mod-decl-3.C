
export void Foo (); // { dg-error "after an interface module" }

module bob [[interface]];
// { dg-module-if "!bob" }

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
