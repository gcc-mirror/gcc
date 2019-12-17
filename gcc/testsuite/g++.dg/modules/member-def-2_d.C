// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

import foo;

int main ()
{
  frob x;
  x.member ();
}

// { dg-final { scan-lang-dump {Reading function definition '::frob@foo:1::member@foo:1'} module } }

// { dg-final { scan-assembler {_ZN4frob6memberEv:} } }
