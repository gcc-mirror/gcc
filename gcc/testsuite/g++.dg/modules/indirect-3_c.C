// { dg-additional-options -fdump-lang-module }
import bar;

int main ()
{

  if (bar::quux ())
    return 1;

  if (bar::toto ())
    return 2;

  return 0;
}

// { dg-final { scan-lang-dump {Lazily loading '::bar::quux'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily loading '::foo::X'@'foo' section} module } }
// { dg-final { scan-lang-dump {Imported:-31 template_decl:'::foo::X::frob'@foo} module } }
// { dg-final { scan-lang-dump {Instantiation:-33 function_decl:'::foo::X::frob'@foo} module } }

// { dg-final { scan-lang-dump {Lazily loading '::bar::toto'@'bar' section:} module } }
// { dg-final { scan-lang-dump {>Lazily loading '::foo::TPL'@'foo' section} module } }

// { dg-final { scan-lang-dump {Imported:-17 template_decl:'::foo::TPL'@foo} module } }
// { dg-final { scan-lang-dump {Imported:-33 function_decl:'::foo::TPL::frob'@foo} module } }
// { dg-final { scan-lang-dump {Instantiation:-20 type_decl:'::foo::TPL'@foo} module } }
