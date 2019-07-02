// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks-alias-uid" }
export module builtin;
// { dg-module-cmi builtin }

export inline void ary_del (int *ptr)
{
  delete[] ptr;
}

export inline void scalar_del (int *ptr)
{
  delete ptr;
}

// { dg-final { scan-lang-dump {Wrote GMF:-[0-9]* function_decl:'::operator delete'@builtin} module } }
// { dg-final { scan-lang-dump {Wrote GMF:-[0-9]* function_decl:'::operator delete \[\]'@builtin} module } }

// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl declaration '::operator delete'\n  \[1\]=binding '::operator delete'} module } }
// { dg-final { scan-lang-dump {Writing:-[0-9]* function_decl:'::operator delete'\n Writing key for mergeable decl function_decl:'::operator delete'} module } }
