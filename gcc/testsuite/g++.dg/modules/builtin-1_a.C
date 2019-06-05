// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks-alias-uid" }
export module builtin;
// { dg-module-bmi builtin }

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

// { dg-final { scan-lang-dump {Cluster:1 2 depsets\n  \[0\]=decl declaration '::operator delete'\n  \[1\]=binding '::operator delete'} module } }
// { dg-final { scan-lang-dump {Wrote:-[0-9]* global decl function_decl:'::operator delete'\n( *Wr[^\n]*\n){5} Seeded 1 mergeables} module } }
