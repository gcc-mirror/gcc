// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-cmi !foo }

namespace bob {

export void corge ();
void grault ();

export class Q
{
  friend void foo ();
  friend void bar ();
  friend void corge ();
  friend void grault ();
};

export void foo ();
void bar (); // exported

class R
{
  friend void quux ();
  friend void toto ();
  friend void corge ();
  friend void grault ();
};

export void quux (); // { dg-error "conflicting export" }
void toto (); // not exported

}
// { dg-prune-output "not writing module" }
