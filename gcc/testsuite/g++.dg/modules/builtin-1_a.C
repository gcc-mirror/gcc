// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }
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

// { dg-final { scan-lang-dump {Wrote builtin:-[0-9]* function_decl:'::operator delete'@builtin} module } }
// { dg-final { scan-lang-dump {Wrote builtin:-[0-9]* function_decl:'::operator delete \[\]'@builtin} module } }
