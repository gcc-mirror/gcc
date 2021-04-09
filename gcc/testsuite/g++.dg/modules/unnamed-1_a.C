// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

export module PiL;
// { dg-module-cmi PiL }

int counter = 0;

export inline int get ()
{
  return counter++;
}

export inline int hwm ()
{
  return counter;
}

// { dg-final { scan-lang-dump {Bindings '::counter' section:1} module } }
// { dg-final { scan-lang-dump-not {horcrux} module } }
// { dg-final { scan-lang-dump-not {Unnamed . '::counter'} module } }
