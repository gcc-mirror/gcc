// { dg-additional-options "-fmodules-ts -fdump-lang-module" }
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
// { dg-final { scan-lang-dump {Imported:-[0-9]* template_decl:'::foo::X::frob'@foo} module } }
// { dg-final { scan-lang-dump {Instantiation:-[0-9]* function_decl:'::foo::X::frob'@foo} module } }

// { dg-final { scan-lang-dump {Lazily binding '::bar::toto'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily binding '::foo::TPL'@'foo' section} module } }

// { dg-final { scan-lang-dump {Imported:-[0-9]* template_decl:'::foo::TPL'@foo} module } }
// { dg-final { scan-lang-dump {Imported:-[0-9]* function_decl:'::foo::TPL::frob'@foo} module } }
// { dg-final { scan-lang-dump {Instantiation:-[0-9]* type_decl:'::foo::TPL'@foo} module } }
