// { dg-additional-options {-fmodules-ts -fdump-lang-module} }

import TPL;

int main ()
{
  frob f;

  f.store (1);
  if (f.i != -1)
    return 1;

  f.store (1.0f);
  if (f.i != 1)
    return 2;

  return 0;
}

// We read a pending for both '::frob' and '::frob::store'.
// { dg-final { scan-lang-dump {Reading 2 pending entities keyed to '::frob'} module } }
// { dg-final { scan-lang-dump-not {Reading definition function_decl '::frob@TPL:.::store@TPL:.<int>'} module } }

// { dg-final { scan-assembler-not {_ZN4frob5storeIiEEvT_:} } }

