// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

import foo;

void f ()
{
  auto f = foo ();

  decltype (f)::inner x;
}
