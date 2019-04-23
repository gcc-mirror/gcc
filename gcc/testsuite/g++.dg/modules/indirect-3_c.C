// { dg-additional-options "-fmodules-ts -fdump-lang-module-uid" }
import bar;

int main ()
{

  if (bar::quux ())
    return 1;

  if (bar::toto ())
    return 2;

  return 0;
}

// { dg-final { scan-lang-dump {Lazily binding '::bar::quux'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily binding '::foo::X'@'foo' section} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* template_decl:'::foo::X@2\(foo\)::frob'@foo} module } }
// { dg-final { scan-lang-dump-not {Instantiation:-[0-9]* function_decl:'::foo::X@2\(foo\)::frob@1()<0x0>'} module } }

// { dg-final { scan-lang-dump {Lazily binding '::bar::toto'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily binding '::foo::TPL'@'foo' section} module } }

// { dg-final { scan-lang-dump {Imported:-[0-9]* template_decl:'::foo::TPL@2\(foo\)'@foo} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* function_decl:'::foo::TPL@2\(foo\)<0x0>::frob<0x0>'@\(detached\)} module } }
// { dg-final { scan-lang-dump {Instantiation:-[0-9]* type_decl:'::foo::TPL@2\(foo\)<0x0>'@foo} module } }
