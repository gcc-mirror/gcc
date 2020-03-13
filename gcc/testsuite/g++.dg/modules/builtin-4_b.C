// { dg-additional-options "-fmodules-ts -fno-module-lazy -fdump-lang-module-alias" }

import "builtin-4_a.H";

int main ()
{
  operator delete (operator new (10));
  operator delete[] (operator new[] (10));
}

// { dg-final { scan-lang-dump {named merge key \(matched\) function_decl:'::operator new'} module } }
// { dg-final { scan-lang-dump {named merge key \(matched\) function_decl:'::operator delete'} module } }
// { dg-final { scan-lang-dump {named merge key \(matched\) function_decl:'::operator new \[\]'} module } }
// { dg-final { scan-lang-dump {named merge key \(matched\) function_decl:'::operator delete \[\]'} module } }
