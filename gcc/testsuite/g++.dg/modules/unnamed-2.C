// { dg-additional-options "-fmodules-ts -fdump-lang-module" }
export module PiL;
// { dg-module-cmi PiL }

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

// { dg-final { scan-lang-dump-not {Bindings '::counter' section:} module } }
// { dg-final { scan-lang-dump-not {Unnamed . '::counter'} module } }
