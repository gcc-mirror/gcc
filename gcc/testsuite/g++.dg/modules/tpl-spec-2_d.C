// { dg-additional-options {-fmodules-ts -fdump-lang-module} }

import TPL;

int one ()
{
  if (foo (1.0f) != 1)
    return 1;
  return 0;
}


import SPEC;

int two ()
{
  if (foo (1) != 0)
    return 2;

  return 0;
}

// { dg-final { scan-lang-dump {Reading 1 pending specializations keyed to TPL\[0\] '::template foo@TPL:.'} module } }
// { dg-final { scan-lang-dump-not {Reading definition function_decl '::foo@TPL:.<int>'} module } }

// { dg-final { scan-assembler-not {_Z3fooIiEiT_:} } }
// { dg-final { scan-assembler {_Z3fooIfEiT_:} } }
