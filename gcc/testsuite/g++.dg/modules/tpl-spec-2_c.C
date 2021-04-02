// { dg-additional-options {-fmodules-ts -fdump-lang-module} }

import SPEC;

int main ()
{
  if (foo (1.0f) != 1)
    return 1;

  if (foo (1) != 0)
    return 2;

  return 0;
}

// { dg-final { scan-lang-dump {Reading 1 pending entities keyed to '::foo'} module } }
// { dg-final { scan-lang-dump-not {Reading definition function_decl '::foo@TPL:.<int>'} module } }

// { dg-final { scan-assembler-not {_Z3fooIiEiT_:} } }
