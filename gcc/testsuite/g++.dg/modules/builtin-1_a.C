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

// { dg-final { scan-lang-dump {Writing named:-[0-9]* function_decl:'::operator delete'\n *Wrote[^\n]*\n *Writing:-[0-9]*'s named merge key \(decl\) function_decl:'::operator delete'} module } }
