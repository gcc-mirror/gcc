// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }
import builtin;

int main ()
{
  ary_del (nullptr);
  scalar_del (nullptr);
  return 0;
}

// { dg-final { scan-lang-dump {Builtin:-[0-9]* function_decl:'::operator delete \[\]'@\(detached\)} module } }
// { dg-final { scan-lang-dump {Builtin:-[0-9]* function_decl:'::operator delete'@\(detached\)} module } }
