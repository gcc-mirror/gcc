// { dg-additional-options "-fmodules-ts -fdump-lang-module" }
export module PiL;
// { dg-module-bmi PiL }

static int counter = 0;

// These are not inlined, so their bodies don't get into the BMI
export int get ()
{
  return counter++;
}

export int hwm ()
{
  return counter;
}

// Ideally we wouldn't write this binding, because it's not reachable
// from anywhere else
// { dg-final { scan-lang-dump {Bindings '::counter' section:} module } }
// { dg-final { scan-lang-dump-not {Unnamed . '::counter'} module } }
