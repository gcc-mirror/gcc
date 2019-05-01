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

// { dg-final { scan-lang-dump {Reading 1 pending specializations keyed to '::frob@TPL:2'} module } }
// { dg-final { scan-lang-dump-not {Reading definition function_decl '::frob@TPL:1::store@TPL:2<int>'} module } }

// { dg-final { scan-assembler-not {_ZN4frob5storeIiEEvT_:} } }

