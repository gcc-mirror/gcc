// { dg-additional-options "-fdump-lang-module" }
export module PiL;
// { dg-module-bmi PiL }

static int counter = 0;

export inline int get ()
{
  return counter++;
}

export inline int hwm ()
{
  return counter;
}

// { dg-final { scan-lang-dump {Unnamed 0 '::counter'} module } }
// { dg-final { scan-lang-dump {Created horcrux:0 for '::counter'} module } }
// { dg-final { scan-lang-dump {Wrote backref:-1 var_decl:'::counter'} module } }
// { dg-final { scan-lang-dump {Unnamed 0 '::counter' section:} module } }
// { dg-final { scan-lang-dump {Unnamed 1 decl} module } }
