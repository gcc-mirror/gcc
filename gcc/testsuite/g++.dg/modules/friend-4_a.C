// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-cmi foo }

namespace bob {

export void corge ();
void grault ();

export class Q
{
  friend void foo ();
  friend void bar ();
  friend void corge ();
  friend void grault ();
  friend void xyzzy (Q);
};

export void foo ();
void bar (); // exported

class R
{
  friend void toto ();
  friend void corge ();
  friend void grault ();
  friend void xyzzy (R);
};

void toto (); // not exported
export R getR ();
}
