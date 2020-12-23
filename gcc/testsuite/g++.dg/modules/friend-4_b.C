// { dg-additional-options -fmodules-ts }

import foo;

using namespace bob;

void doit ()
{
  corge ();
  foo ();
  bar ();

  grault (); // { dg-error "not declared" }
  toto (); // { dg-error "not declared" }
  xyzzy (); // { dg-error "not declared" }

  xyzzy (getR);  // ADL
  xyzzy (Q{});  // ADL
}
