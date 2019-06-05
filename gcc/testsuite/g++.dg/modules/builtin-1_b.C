// { dg-additional-options "-fmodules-ts -fdump-lang-module-alias-uid" }
import builtin;

int main ()
{
  ary_del (nullptr);
  scalar_del (nullptr);
  return 0;
}

// { dg-final { scan-lang-dump {Lazily binding '::operator delete \[\]'@'builtin' section:.} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]* matched mergeable decl function_decl:'::operator delete \[\]'} module } }
// { dg-final { scan-lang-dump {Lazily binding '::operator delete'@'builtin' section:.} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]* matched mergeable decl function_decl:'::operator delete'} module } }
