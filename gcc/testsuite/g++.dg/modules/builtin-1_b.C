// { dg-additional-options "-fmodules-ts -fdump-lang-module-alias" }
import builtin;

int main ()
{
  ary_del (nullptr);
  scalar_del (nullptr);
  return 0;
}

// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) function_decl:'::operator delete \[\]'} module } }
// { dg-final { scan-lang-dump {Read:-[0-9]*'s named merge key \(matched\) function_decl:'::operator delete'} module } }
