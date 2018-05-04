// { dg-additional-options "-fdump-lang-module" }
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

// { dg-final { scan-lang-dump-not {Unnamed 0 '::counter'} module } }
// { dg-final { scan-lang-dump-not {Wrote voldemort:0 var_decl:'::counter'} module } }
// { dg-final { scan-lang-dump-not {Unnamed 0 '::counter' section:} module } }
// { dg-final { scan-lang-dump-not {Unnamed 1 decl} module } }
// { dg-final { scan-lang-dump-not {Declaration '::counter'} module } }
