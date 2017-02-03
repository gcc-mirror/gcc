
extern module foo : void Baz ();

extern module  : void Bar (); // { dg-error "expected identifier" }
extern module funk void Quux (); // { dg-error "expected ':'" }

module me [[interface]];
// { dg-module-if "!me" }

export extern module foo : void Toto (); // { dg-error "may only occur" }
extern module foo : export void Bink (); // { dg-error "may not occur" }

namespace 
{
  extern module foo : void Baz (); // { dg-error "may only occur" }
}
