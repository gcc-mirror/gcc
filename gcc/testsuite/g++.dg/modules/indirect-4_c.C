// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }
import bar;

int main ()
{
  if (bar::quux () != 3)
    return 1;

  return 0;
}

// { dg-final { scan-lang-dump {Lazily binding '::bar::quux'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily binding '::foo::TPL'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* template_decl:'::foo::TPL@\(foo\)'@foo} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* template_decl:'::foo::TPL@\(foo\)<0x1>::frob<#unnamed#>'@foo} module } }
// { dg-final { scan-lang-dump {Instantiation:-[0-9]* type_decl:'::foo::TPL@\(foo\)<0x1>'@foo} module } }
// { dg-final { scan-lang-dump {Instantiation:-[0-9]* function_decl:'::foo::TPL@\(foo\)<0x1>::frob<0x2>'@\(detached\)} module } }
